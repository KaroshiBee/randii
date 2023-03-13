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
       * error if no number is found *)
      Array.to_list r
      |> List.filter (fun x -> x <= limit upper)
      |> List.map (fun x -> U.rem x upper |> U.to_string)
      |> Array.of_list
      |> Ctr.of_string_array
    with
    | Invalid_argument _ -> Result.error @@ `Error (Printf.sprintf "Bad ctr/key pair for given ~upper:%s" @@ U.to_string upper)

  let rand2 ~key ~ctr =
    match (Ctr.digits ctr, Ctr.digits key) with
    | (Two, Two) ->
      Rng.rand (Ctr.data key) (Ctr.data ctr)
      |> Array.map U.to_string
      |> Ctr.of_string_array
    | _ -> Result.error @@ `Error "Need two digit ctr/key"

  let uniform2 ~(upper:int) ~key ~ctr =
    let upper = U.of_int upper in
    if U.equal U.zero upper then Result.error @@ `Error "zero upper" else
      match (Ctr.digits ctr, Ctr.digits key) with
      | (Two, Two) ->
        Rng.rand (Ctr.data key) (Ctr.data ctr)
        |> unbiased upper
      | _ -> Result.error @@ `Error "Need two digit ctr/key"

  let rand4 ~key ~ctr =
    match (Ctr.digits ctr, Ctr.digits key) with
    | (Four, Four) ->
      Rng.rand (Ctr.data key) (Ctr.data ctr)
      |> Array.map U.to_string
      |> Ctr.of_string_array
    | _ -> Result.error @@ `Error "Need four digit ctr/key"

  let uniform4 ~(upper:int) ~key ~ctr =
    let upper = U.of_int upper in
    if U.equal U.zero upper then Result.error @@ `Error "zero upper" else
      match (Ctr.digits ctr, Ctr.digits key) with
      | (Four, Four) ->
        Rng.rand (Ctr.data key) (Ctr.data ctr)
        |> unbiased upper
      | _ -> Result.error @@ `Error "Need four digit ctr/key"

end

module type DISCRETE = sig
  type t

  val incr : t -> t
  val rand : key:t -> ctr:t -> (t, Errors.t) Result.t
  val uniform : upper:int -> key:t -> ctr:t -> (t, Errors.t) Result.t

  val to_string_array : t -> string array
  val of_string_array : string array -> (t, Errors.t) Result.t
end

module Make_discrete2xW (U:Threefry.T2) : DISCRETE = struct

  module Ctr = Ctr.Make_ctr(U)
  module Rng = Threefry.Make_threefry2xW(U)

  open Maker_(U) (Ctr) (Rng)

  type t = Ctr.t

  let incr = Ctr.succ
  let rand = rand2
  let uniform = uniform2

  let to_string_array = Ctr.to_string_array
  let of_string_array = Ctr.of_string_array
end

module Make_discrete4xW (U:Threefry.T4) : DISCRETE = struct

  module Ctr = Ctr.Make_ctr(U)
  module Rng = Threefry.Make_threefry4xW(U)

  open Maker_(U) (Ctr) (Rng)

  type t = Ctr.t

  let incr = Ctr.succ
  let rand = rand4
  let uniform = uniform4

  let to_string_array = Ctr.to_string_array
  let of_string_array = Ctr.of_string_array
end

module Threefry_2x32 = Make_discrete2xW(Threefry.UInt32_2_T)
module Threefry_2x64 = Make_discrete2xW(Threefry.UInt64_2_T)
module Threefry_4x32 = Make_discrete4xW(Threefry.UInt32_4_T)
module Threefry_4x64 = Make_discrete4xW(Threefry.UInt64_4_T)

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

type kind =
    Rand
  | Uniform of int


let gen ~rng_name_arg ?key_arg ?ctr_arg n kind =
  let (let*) = Result.bind in
  (* default key/ctr values *)
  let key2 = [|"0";"1"|] in
  let key4 = [|"0";"1";"2";"3"|] in
  let ctr2 = [|"0";"1"|] in
  let ctr4 = [|"0";"1";"2";"3"|] in

  let* {word_size; digits; algo} = RngName.of_string rng_name_arg in
  let rand ?key_arg ?ctr_arg = function
    | Word_size.ThirtyTwo, Digits.Two, Algo.Threefry -> Threefry_2x32.(
        let* key = of_string_array @@ Option.value key_arg ~default:key2 in
        let* ctr = of_string_array @@ Option.value ctr_arg ~default:ctr2 in
        let* arr = match kind with Rand -> rand ~key ~ctr | Uniform upper -> uniform ~upper ~key ~ctr in
        Result.ok (to_string_array arr, incr ctr |> to_string_array)
      )
    | Word_size.ThirtyTwo, Digits.Four, Algo.Threefry -> Threefry_4x32.(
        let* key = of_string_array @@ Option.value key_arg ~default:key4 in
        let* ctr = of_string_array @@ Option.value ctr_arg ~default:ctr4 in
        let* arr = match kind with Rand -> rand ~key ~ctr | Uniform upper -> uniform ~upper ~key ~ctr in
        Result.ok (to_string_array arr, incr ctr |> to_string_array)
      )
    | Word_size.SixtyFour, Digits.Two, Algo.Threefry -> Threefry_2x64.(
        let* key = of_string_array @@ Option.value key_arg ~default:key2 in
        let* ctr = of_string_array @@ Option.value ctr_arg ~default:ctr2 in
        let* arr = match kind with Rand -> rand ~key ~ctr | Uniform upper -> uniform ~upper ~key ~ctr in
        Result.ok (to_string_array arr, incr ctr |> to_string_array)
      )
    | Word_size.SixtyFour, Digits.Four, Algo.Threefry -> Threefry_4x64.(
        let* key = of_string_array @@ Option.value key_arg ~default:key4 in
        let* ctr = of_string_array @@ Option.value ctr_arg ~default:ctr4 in
        let* arr = match kind with Rand -> rand ~key ~ctr | Uniform upper -> uniform ~upper ~key ~ctr in
        Result.ok (to_string_array arr, incr ctr |> to_string_array)
      )
  in
  let rec aux :
    ?key_arg:string array
    -> ?ctr_arg:string array
    -> int
    -> string array list
    -> (string array list, Errors.t) result
    = fun ?key_arg ?ctr_arg i acc ->
      if i > 0 then
        let* arr, ctr = rand ?key_arg ?ctr_arg (word_size, digits, algo) in
        let m = Array.length arr in
        let ctr_arg = Option.some ctr in
        aux ?key_arg ?ctr_arg (i-m) (arr :: acc)
      else
        Result.ok @@ List.rev acc
  in
  let* arr = aux ?key_arg ?ctr_arg n [] in
  let arr = Array.concat arr in
  Result.ok @@ Array.sub arr 0 n
