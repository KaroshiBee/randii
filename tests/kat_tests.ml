(* KAT == Known Answer Tests
 * data is taken from Random123 test/ dir *)
open Randii.Threefry
module R2x32 = Make_threefry2xW_TEST(UInt32_2_T)
module R2x64 = Make_threefry2xW_TEST(UInt64_2_T)

module R4x32 = Make_threefry4xW_TEST(UInt32_4_T)
module R4x64 = Make_threefry4xW_TEST(UInt64_4_T)

module I32 = Unsigned.UInt32
module I64 = Unsigned.UInt64

module Word_size = Randii.Rng.Word_size
module Digits = Randii.Rng.Digits
module Algo = Randii.Rng.Algo
module RngName = Randii.Rng.RngName

module RngData = struct
  type t = {
    name: RngName.t;
    nrounds: int;
    ctr: string array;
    key: string array;
    expected: string array
  }

  (* all the numbers in the KAT data files are hex 32/64 bit
   * but without the 0x on the front,
   * so read them in with zarith arb precision
   * and then output back to arb precision integer strings base 10 *)
  let _read_hex s = "0x" ^ s |> Z.of_string |> Z.to_string

  let of_string s =
    let (let*) = Result.bind in
    let parts = String.split_on_char ' ' s
                |> List.map String.trim
                |> List.filter (fun ss -> String.length ss > 0)
                |> Array.of_list
    in
    let* name = parts.(0) |> RngName.of_string in
    let nrounds = parts.(1) |> int_of_string in
    let n = Digits.length name.digits in
    let ctr = Array.sub parts 2 n |> Array.map _read_hex in
    let key = Array.sub parts (2+n) n |> Array.map _read_hex in
    let expected = Array.sub parts (2+n+n) n |> Array.map _read_hex in
    Result.ok {name; nrounds; ctr; key; expected}

  let _to_string {name; nrounds; ctr; key; expected} =
    Array.concat [
      [| RngName.to_string name; string_of_int nrounds |];
      ctr;
      key;
      expected;
    ]
    |> Array.to_list
    |> String.concat " "

end

let read_lines name : string list =
  let () = Logs.info (fun m -> m "reading KAT file: %s" name) in
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let read_kat_data ln =
  match RngData.of_string ln with
  | Result.Ok v -> Some v
  | Result.Error e -> let () = Logs.warn (fun m -> m "Error: %s\nLine: %s" (Randii.Errors.to_string e) ln) in None

let test_kat_data i rng_data () =
  let RngData.{name; nrounds; ctr; key; expected} = rng_data in
  let actual = match (name.digits, name.word_size) with
    | (Two,  ThirtyTwo) -> R2x32.rand_R ~rounds:nrounds ~key:(key |> Array.map I32.of_string) ~ctr:(ctr |> Array.map I32.of_string)
                           |> Array.map I32.to_string
    | (Two,  SixtyFour) -> R2x64.rand_R ~rounds:nrounds ~key:(key |> Array.map I64.of_string) ~ctr:(ctr |> Array.map I64.of_string)
                           |> Array.map I64.to_string
    | (Four, ThirtyTwo) -> R4x32.rand_R ~rounds:nrounds ~key:(key |> Array.map I32.of_string) ~ctr:(ctr |> Array.map I32.of_string)
                           |> Array.map I32.to_string
    | (Four, SixtyFour) -> R4x64.rand_R ~rounds:nrounds ~key:(key |> Array.map I64.of_string) ~ctr:(ctr |> Array.map I64.of_string)
                           |> Array.map I64.to_string
  in
  Alcotest.(check (array string))
    (RngName.to_string name ^ (string_of_int i))
    expected actual

(* Run it *)
let () =
  let kat_data = read_lines "data/kat_vectors"
                 |> List.map read_kat_data
                 |> List.filter Option.is_some
                 |> List.map Option.get
  in
  let kat_data_old = read_lines "data/old_kat_vectors"
                     |> List.map read_kat_data
                     |> List.filter Option.is_some
                     |> List.map Option.get
  in
  Alcotest.run "ThreefryNxW Known Answers"
    [
      ( "kat vectors",
        kat_data |> List.mapi (fun i rng_data ->
            let tt = test_kat_data i rng_data in
            let n = Printf.sprintf "%s: %d rounds" (RngName.to_string rng_data.name) rng_data.nrounds in
            Alcotest.test_case n `Quick tt
          )
      );
      ( "old kat vectors",
        kat_data_old |> List.mapi (fun i rng_data ->
            let tt = test_kat_data i rng_data in
            let n = Printf.sprintf "%s: %d rounds" (RngName.to_string rng_data.name) rng_data.nrounds in
            Alcotest.test_case n `Quick tt
          )
      );
    ]
