(* unif01_Gen *unif01_CreateExternGenBits (char *name, unsigned int (PTR genB)(void));
    Implements a pre-existing external generator genB that is not part of TestU01. It must be a C
    function taking no argument and returning an integer in the interval [0, 2^32 −1]. If the generator
    delivers less than 32 bits of resolution, then these bits must be left shifted so that the most
    significant bit is bit 31 (counting from 0). Parameter name is the name of the generator. No
    more than one generator of this type can be in use at a time *)

(* expose to C the generators *)
open Randii.Rng
let (let*) = Result.bind

let rng_2x32 (k1, k2, c1, c2, index) =
  let key_arg = [|k1; k2|] in
  let ctr_arg = [|c1; c2|] in
  match index, gen ~rng_name_arg:"threefry2x32" ~key_arg ~ctr_arg 2 Rand with
  | 0, Result.Ok [|x; _|] -> Unsigned.UInt32.of_string x
  | 1, Result.Ok [|_; y|] -> Unsigned.UInt32.of_string y
  | _, Result.Ok _ -> raise @@ Invalid_argument "Expected 2 rngs"
  | _, Result.Error _ -> raise @@ Invalid_argument "Errored rng"

let next_2x32 i1 i2 =
  let module T = Threefry_2x32 in
  match T.of_string_array [|i1;i2|] with
  | Result.Ok arr -> (
      match arr |> T.incr |> T.to_string_array with
      | [|x; y|] -> (x, y)
      | _ -> raise @@ Invalid_argument "Errored next"
    )
  | Result.Error _ -> raise @@ Invalid_argument "Errored next"
(* let rng_2x64 = Randii.Rng.(gen ~rng_name_arg:"threefry2x64" 2 Rand) *)
(* let rng_4x32 = Randii.Rng.(gen ~rng_name_arg:"threefry4x32" 4 Rand) *)
(* let rng_4x64 = Randii.Rng.(gen ~rng_name_arg:"threefry4x64" 4 Rand) *)

