(* Need unbiased transform to [0, n]
 * taken from
 * https://stackoverflow.com/questions/10984974/why-do-people-say-there-is-modulo-bias-when-using-a-random-number-generator
 * // Assumptions
 * // rand() in [0, RAND_MAX]
 * // n in (0, RAND_MAX]
 *
 * int x;
 *
 * // Keep searching for an x in a range divisible by n
 * do {
 *     x = rand();
 * } while (x >= RAND_MAX - (RAND_MAX % n))
 *
 * x %= n; *)

module Maker_
    (U:Threefry.T)
    (Ctr:Ctr.CTR with type el := U.t)
    (Rng:Threefry.RAND_T with type ctr_t := U.t array and type key_t := U.t array)
= struct

  let limit n = U.(sub max_int (rem max_int n))

  let unbiased upper r =
    try
      (* find first rand less than limit
       * and do the remainder with that number
       * throws if no number is found *)
      Array.to_list r
      |> List.find_opt (fun x -> x <= limit upper)
      |> Option.map (fun x -> U.rem x upper)
      |> Option.get
      |> Result.ok
    with
    | Invalid_argument _ -> Result.error @@ `Error (Printf.sprintf "Bad ctr/key pair for given ~upper:%s" @@ U.to_string upper)

  let discrete2 ~(upper:U.t) key ctr =
    if U.equal U.zero upper then Result.ok U.zero else
      match (Ctr.digits ctr, Ctr.digits key) with
      | (Two, Two) -> (
          Rng.rand (Ctr.data key) (Ctr.data ctr)
          |> unbiased upper
        )
      | _ -> Result.error @@ `Error "Need two digit ctr/key"

  let discrete4 ~(upper:U.t) key ctr =
    if U.equal U.zero upper then Result.ok U.zero else
      match (Ctr.digits ctr, Ctr.digits key) with
      | (Four, Four) -> (
          Rng.rand (Ctr.data key) (Ctr.data ctr)
          |> unbiased upper
        )
      | _ -> Result.error @@ `Error "Need four digit ctr/key"

end

module type UNIFORM_DISCRETE = sig
  type t
  type ctr
  val uniform : upper:t -> ctr -> ctr -> t
end

module Make_uniform2xW (U:Threefry.T2) = struct

  module Ctr = Ctr.Make_ctr(U)
  module Rng = Threefry.Make_threefry2xW(U)

  open Maker_(U) (Ctr) (Rng)

  let discrete = discrete2
end

module Make_uniform4xW (U:Threefry.T4) = struct

  module Ctr = Ctr.Make_ctr(U)
  module Rng = Threefry.Make_threefry4xW(U)

  open Maker_(U) (Ctr) (Rng)

  let discrete = discrete4

end

module R2x32 = Threefry.Make_threefry2xW(Threefry.UInt32_2_T)
module R2x64 = Threefry.Make_threefry2xW(Threefry.UInt64_2_T)
module U2x32 = Make_uniform2xW(Threefry.UInt32_2_T)
module U2x64 = Make_uniform2xW(Threefry.UInt64_2_T)

module R4x32 = Threefry.Make_threefry4xW(Threefry.UInt32_4_T)
module R4x64 = Threefry.Make_threefry4xW(Threefry.UInt64_4_T)
module U4x32 = Make_uniform4xW(Threefry.UInt32_4_T)
module U4x64 = Make_uniform4xW(Threefry.UInt64_4_T)

module I32 = Unsigned.UInt32
module I64 = Unsigned.UInt64

module Word_size = struct
  type t = | ThirtyTwo | SixtyFour

  let of_string = function
    | "32" -> Result.ok ThirtyTwo
    | "64" -> Result.ok SixtyFour
    | s -> Result.error @@ `Unknown_word_size s

  let to_string = function
    | ThirtyTwo -> "32"
    | SixtyFour -> "64"

  let length = function
    | ThirtyTwo | SixtyFour -> 2

end

module Digits = struct
  type t = | Two | Four

  let of_string = function
    | "2" -> Result.ok Two
    | "4" -> Result.ok Four
    | s -> Result.error @@ `Unknown_digits s

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
    | "threefry" -> Result.ok Threefry
    | s -> Result.error @@ `Unknown_algo s

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
    let (let*) = Result.bind in
    match (String.lowercase_ascii s |> String.split_on_char 'x') with
    | [left; right] ->
      let* word_size = Word_size.of_string right in
      let n = Algo.(Threefry |> length) in
      let* algo = (try String.sub left 0 n with Invalid_argument _ -> "") |> Algo.of_string in
      let* digits = (try String.sub left n 1 with Invalid_argument _ -> "") |> Digits.of_string in
      Result.ok {word_size; digits; algo}
    | _ -> Result.error @@ `Unknown_generator s

  let to_string {word_size; digits; algo} =
    Algo.to_string algo ^
    Digits.to_string digits ^ "x" ^
    Word_size.to_string word_size

  let length t = to_string t |> String.length

end
