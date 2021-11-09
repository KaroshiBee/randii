(* KAT == Known Answer Tests
 * data is taken from Random123 test/ dir *)

module U2x32 = Randii.Threefry.UInt32_2_T
module U2x64 = Randii.Threefry.UInt64_2_T
module R2x32 = Randii.Threefry.Make_threefry2xW_TEST(U2x32)
module R2x64 = Randii.Threefry.Make_threefry2xW_TEST(U2x64)

module U4x32 = Randii.Threefry.UInt32_4_T
module U4x64 = Randii.Threefry.UInt64_4_T
module R4x32 = Randii.Threefry.Make_threefry4xW_TEST(U4x32)
module R4x64 = Randii.Threefry.Make_threefry4xW_TEST(U4x64)

module I32 = Unsigned.UInt32
module I64 = Unsigned.UInt64

module Word_size = struct
  type t = | ThirtyTwo | SixtyFour

  let of_string = function
    | "32" -> ThirtyTwo
    | "64" -> SixtyFour
    | _ -> raise (Invalid_argument "Unknown word size")

  let to_string = function
    | ThirtyTwo -> "32"
    | SixtyFour -> "64"

  let length = function
    | ThirtyTwo | SixtyFour -> 2

end

module Digits = struct
  type t = | Two | Four

  let of_string = function
    | "2" -> Two
    | "4" -> Four
    | _ -> raise (Invalid_argument "Unknown digits")

  let to_string = function
    | Two -> "2"
    | Four -> "4"

  let length = function
    | Two -> 2
    | Four -> 4

end

module Algo = struct
  type t = | Threefry

  let of_string = function
    | "threefry" -> Threefry
    | _ -> raise (Invalid_argument "Unknown algo")

  let to_string = function
    | Threefry -> "threefry"

  let length t = to_string t |> String.length

end

module RngName = struct

  type t = {
    word_size:Word_size.t;
    digits:Digits.t;
    algo:Algo.t;
  }

  let of_string s =
    match (String.lowercase_ascii s |> String.split_on_char 'x') with
    | [left; right] ->
      let word_size = Word_size.of_string right in
      let n = Algo.(Threefry |> length) in
      let algo = String.sub left 0 n |> Algo.of_string in
      let digits = String.sub left n 1 |> Digits.of_string in
      {word_size; digits; algo}
    | _ -> raise (Invalid_argument "Unknown generator")

  let to_string {word_size; digits; algo} =
    Algo.to_string algo ^
    Digits.to_string digits ^ "x" ^
    Word_size.to_string word_size

  let length t = to_string t |> String.length

end

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
    let parts = String.split_on_char ' ' s
                |> List.map String.trim
                |> List.filter (fun ss -> String.length ss > 0)
                |> Array.of_list
    in
    let name = parts.(0) |> RngName.of_string in
    let nrounds = parts.(1) |> int_of_string in
    let n = Digits.length name.digits in
    let ctr = Array.sub parts 2 n |> Array.map _read_hex in
    let key = Array.sub parts (2+n) n |> Array.map _read_hex in
    let expected = Array.sub parts (2+n+n) n |> Array.map _read_hex in
    {name; nrounds; ctr; key; expected}

  let to_string {name; nrounds; ctr; key; expected} =
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
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let read_kat_data ln =
  try
    Some (RngData.of_string ln)
  with Invalid_argument _ -> None
(* with Invalid_argument s -> match s with
 *   | "Unknown generator" -> None
 *   | s ->
 *     Printf.printf "ERROR: %s\n" s;
 *     None *)

let test_kat_data i rng_data () =
  let RngData.{name; nrounds; ctr; key; expected} = rng_data in
  let actual = match (name.digits, name.word_size) with
    | (Two,  ThirtyTwo) -> R2x32.rand_R nrounds (key |> Array.map I32.of_string) (ctr |> Array.map I32.of_string)
                           |> Array.map I32.to_string
    | (Two,  SixtyFour) -> R2x64.rand_R nrounds (key |> Array.map I64.of_string) (ctr |> Array.map I64.of_string)
                           |> Array.map I64.to_string
    | (Four, ThirtyTwo) -> R4x32.rand_R nrounds (key |> Array.map I32.of_string) (ctr |> Array.map I32.of_string)
                           |> Array.map I32.to_string
    | (Four, SixtyFour) -> R4x64.rand_R nrounds (key |> Array.map I64.of_string) (ctr |> Array.map I64.of_string)
                           |> Array.map I64.to_string
  in
  Alcotest.(check (array string))
    (RngName.to_string name ^ (string_of_int i))
    expected actual

(* Run it *)
let () =
  let kat_data = read_lines "kat_vectors"
                 |> List.map read_kat_data
                 |> List.filter Option.is_some
                 |> List.map Option.get
  in
  let kat_data_old = read_lines "old_kat_vectors"
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
