(* KAT == Known Answer Tests
 * data is taken from Random123 test/ dir *)
open Cbrn.Threefry

module Word_size = Cbrn.Rng.Word_size
module Digits = Cbrn.Rng.Digits
module Algo = Cbrn.Rng.Algo
module RngName = Cbrn.Rng.RngName

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
  | Result.Error e -> let () = Logs.warn (fun m -> m "Error: %s\nLine: %s" (Cbrn.Errors.to_string e) ln) in None

let test_kat_data i rng_data () =
  let RngData.{name; nrounds; ctr; key; expected} = rng_data in
  let actual = match (name.digits, name.word_size) with
    | (Two,  ThirtyTwo) -> Gen_2_32.(
        rand ~rounds:nrounds ~key:(of_string_array key) ~ctr:(of_string_array ctr) ()
        |> to_string_array
      )
    | (Two,  SixtyFour) -> Gen_2_64.(
        rand ~rounds:nrounds ~key:(of_string_array key) ~ctr:(of_string_array ctr) ()
        |> to_string_array
      )
    | (Four, ThirtyTwo) -> Gen_4_32.(
        rand ~rounds:nrounds ~key:(of_string_array key) ~ctr:(of_string_array ctr) ()
        |> to_string_array
      )
    | (Four, SixtyFour) -> Gen_4_64.(
        rand ~rounds:nrounds ~key:(of_string_array key) ~ctr:(of_string_array ctr) ()
        |> to_string_array
      )
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
