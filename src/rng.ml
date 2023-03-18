module Threefry_2x32 = Threefry.Gen_2_32
module Threefry_2x64 = Threefry.Gen_2_64
module Threefry_4x32 = Threefry.Gen_4_32
module Threefry_4x64 = Threefry.Gen_4_64

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


let gen ~rng_name_arg ?key_arg ?ctr_arg n kind =
  let (let*) = Result.bind in
  (* default key/ctr values *)
  let key2 = [|"0";"1"|] in
  let key4 = [|"0";"1";"2";"3"|] in
  let ctr2 = [|"0";"1"|] in
  let ctr4 = [|"0";"1";"2";"3"|] in

  let* {word_size; digits; algo} = RngName.of_string rng_name_arg in
  let rand ?key_arg ?ctr_arg =
    let _convert arg converter ~default =
      try
        Result.ok @@ converter @@ Option.value arg ~default
      with
      | Invalid_argument s ->
        Result.error @@ `Error s
    in
    function
    | Word_size.ThirtyTwo, Digits.Two, Algo.Threefry -> Threefry_2x32.(
        let* key = _convert key_arg of_string_array ~default:key2 in
        let* ctr = _convert ctr_arg of_string_array ~default:ctr2 in
        let arr = draw_from ~rand ~uniform01 ~uniform ~key ~ctr kind in
        Result.ok (arr, succ ctr |> to_string_array)
      )
    | Word_size.ThirtyTwo, Digits.Four, Algo.Threefry -> Threefry_4x32.(
        let* key = _convert key_arg of_string_array ~default:key4 in
        let* ctr = _convert ctr_arg of_string_array ~default:ctr4 in
        let arr = draw_from ~rand ~uniform01 ~uniform ~key ~ctr kind in
        Result.ok (arr, succ ctr |> to_string_array)
      )
    | Word_size.SixtyFour, Digits.Two, Algo.Threefry -> Threefry_2x64.(
        let* key = _convert key_arg of_string_array ~default:key2 in
        let* ctr = _convert ctr_arg of_string_array ~default:ctr2 in
        let arr = draw_from ~rand ~uniform01 ~uniform ~key ~ctr kind in
        Result.ok (arr, succ ctr |> to_string_array)
      )
    | Word_size.SixtyFour, Digits.Four, Algo.Threefry -> Threefry_4x64.(
        let* key = _convert key_arg of_string_array ~default:key4 in
        let* ctr = _convert ctr_arg of_string_array ~default:ctr4 in
        let arr = draw_from ~rand ~uniform01 ~uniform ~key ~ctr kind in
        Result.ok (arr, succ ctr |> to_string_array)
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
