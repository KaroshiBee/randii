type digits_two
type digits_four
type word_32
type word_64

let rec ctr ~equal digit op sentinal t =
  let n = Array.length t in
  if digit == n then () else (
    let d = op t.(digit) in
    match (equal sentinal d) with
    | false -> t.(digit) <- d;
    | true -> t.(digit) <- d; ctr ~equal (digit+1) op sentinal t
  )


(* factor out dependence on UInt32 / UInt64 and word size (32/64 bit) *)
module type NUM = sig
  type ('digits, 'word) t (* UInt32 or UInt64 *)
  val of_int_2_32 : int -> (digits_two, word_32) t
  val of_int_4_32 : int -> (digits_four, word_32) t
  val of_int_2_64 : int -> (digits_two, word_64) t
  val of_int_4_64 : int -> (digits_four, word_64) t
  val to_int : ('digits, 'word) t -> int

  val of_string_2_32 : string -> (digits_two, word_32) t
  val of_string_4_32 : string -> (digits_four, word_32) t
  val of_string_2_64 : string -> (digits_two, word_64) t
  val of_string_4_64 : string -> (digits_four, word_64) t
  val to_string : ('digits, 'word) t -> string

  val zero : ('digits, 'word) t
  (* val one : ('digits, 'word) t *)
  (* val max_int : ('digits, 'word) t *)

  val equal : ('digits, 'word) t -> ('digits, 'word) t -> bool
  val succ : ('digits, 'word) t -> ('digits, 'word) t
  (* val pred : ('digits, 'word) t -> ('digits, 'word) t *)
  val add : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t
  (* val sub : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t *)
  (* val rem : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t *)
  val logxor : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t

  val skein_ks_parity : ('digits, 'word) t
  val rotations_0 : ('digits, 'word) t Types.Consts.t
  val rotations_1 : ('digits, 'word) t Types.Consts.t
  val rotL : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t

end

module Threefry_2_XX (Num:NUM) = struct

  let aux1 lbound nrounds x0 x1 i =
    (* if(Nrounds>0){  X0 += X1; X1 = RotL_##W(X1,R_##W##x2_0_0); X1 ^= X0; }  *)
    if nrounds>lbound then Num.(
        x0 := add !x0 !x1; x1 := rotL !x1 i; x1 := logxor !x1 !x0;
      )
    else ()

  let aux3 ~of_int lbound nrounds x0 x1 ks0 ks1 i =
    (*
     if(Nrounds>3){                                                      \
        /* InjectKey(r=1) */                                            \
        X0 += ks1; X1 += ks2;                               \
        X1 += 1;     /* X.v[2-1] += r  */                   \
     }                                                                   \
    *)
    if nrounds>lbound then Num.(
        x0 := add !x0 ks0; x1 := add !x1 ks1;
        x1 := add !x1 (of_int i)
      )
    else ()

  let rand_R ~of_int ~rounds:nrounds ~key ~ctr =
    let open Num in
    let max_rounds = 32 in
    let _ = if nrounds > max_rounds then
        failwith @@ Printf.sprintf
          "number rounds must be <= %d" max_rounds
      else ()
    in
    let ks2 = skein_ks_parity in
    let ks0 = key.(0) in
    let x0 = ref (add ctr.(0) ks0) in
    let ks2 = logxor ks2 ks0 in

    let ks1 = key.(1) in
    let x1 = ref (add ctr.(1) ks1) in
    let ks2 = logxor ks2 ks1 in
    let _aux1 = aux1 in
    let _aux3 = aux3 ~of_int in

    let _ = _aux1  0 nrounds x0 x1 rotations_0.i_0 in
    let _ = _aux1  1 nrounds x0 x1 rotations_0.i_1 in
    let _ = _aux1  2 nrounds x0 x1 rotations_0.i_2 in
    let _ = _aux1  3 nrounds x0 x1 rotations_0.i_3 in
    let _ = _aux3  3 nrounds x0 x1 ks1 ks2 1 in

    let _ = _aux1  4 nrounds x0 x1 rotations_0.i_4 in
    let _ = _aux1  5 nrounds x0 x1 rotations_0.i_5 in
    let _ = _aux1  6 nrounds x0 x1 rotations_0.i_6 in
    let _ = _aux1  7 nrounds x0 x1 rotations_0.i_7 in
    let _ = _aux3  7 nrounds x0 x1 ks2 ks0 2 in

    let _ = _aux1  8 nrounds x0 x1 rotations_0.i_0 in
    let _ = _aux1  9 nrounds x0 x1 rotations_0.i_1 in
    let _ = _aux1 10 nrounds x0 x1 rotations_0.i_2 in
    let _ = _aux1 11 nrounds x0 x1 rotations_0.i_3 in
    let _ = _aux3 11 nrounds x0 x1 ks0 ks1 3 in

    let _ = _aux1 12 nrounds x0 x1 rotations_0.i_4 in
    let _ = _aux1 13 nrounds x0 x1 rotations_0.i_5 in
    let _ = _aux1 14 nrounds x0 x1 rotations_0.i_6 in
    let _ = _aux1 15 nrounds x0 x1 rotations_0.i_7 in
    let _ = _aux3 15 nrounds x0 x1 ks1 ks2 4 in

    let _ = _aux1 16 nrounds x0 x1 rotations_0.i_0 in
    let _ = _aux1 17 nrounds x0 x1 rotations_0.i_1 in
    let _ = _aux1 18 nrounds x0 x1 rotations_0.i_2 in
    let _ = _aux1 19 nrounds x0 x1 rotations_0.i_3 in
    let _ = _aux3 19 nrounds x0 x1 ks2 ks0 5 in

    let _ = _aux1 20 nrounds x0 x1 rotations_0.i_4 in
    let _ = _aux1 21 nrounds x0 x1 rotations_0.i_5 in
    let _ = _aux1 22 nrounds x0 x1 rotations_0.i_6 in
    let _ = _aux1 23 nrounds x0 x1 rotations_0.i_7 in
    let _ = _aux3 23 nrounds x0 x1 ks0 ks1 6 in

    let _ = _aux1 24 nrounds x0 x1 rotations_0.i_0 in
    let _ = _aux1 25 nrounds x0 x1 rotations_0.i_1 in
    let _ = _aux1 26 nrounds x0 x1 rotations_0.i_2 in
    let _ = _aux1 27 nrounds x0 x1 rotations_0.i_3 in
    let _ = _aux3 27 nrounds x0 x1 ks1 ks2 7 in

    let _ = _aux1 28 nrounds x0 x1 rotations_0.i_4 in
    let _ = _aux1 29 nrounds x0 x1 rotations_0.i_5 in
    let _ = _aux1 30 nrounds x0 x1 rotations_0.i_6 in
    let _ = _aux1 31 nrounds x0 x1 rotations_0.i_7 in
    let _ = _aux3 31 nrounds x0 x1 ks2 ks0 8 in

    [|!x0; !x1|]
end

module Threefry_4_XX (Num:NUM) = struct

  let aux1 lbound nrounds x0 x1 x2 x3 i_0 i_1 =
    (* if(Nrounds>0){                                                      \
     *     X0 += X1; X1 = RotL_##W(X1,R_##W##x4_0_0); X1 ^= X0; \
     *     X2 += X3; X3 = RotL_##W(X3,R_##W##x4_0_1); X3 ^= X2; \
     * }                                                                   \ *)
    if nrounds>lbound then Num.(
        x0 := add !x0 !x1; x1 := rotL !x1 i_0; x1 := logxor !x1 !x0;
        x2 := add !x2 !x3; x3 := rotL !x3 i_1; x3 := logxor !x3 !x2;
      )
    else ()

  let aux2 lbound nrounds x0 x1 x2 x3 i_0 i_1 =
    (* if(Nrounds>1){                                                      \
     *     X0 += X3; X3 = RotL_##W(X3,R_##W##x4_1_0); X3 ^= X0; \
     *     X2 += X1; X1 = RotL_##W(X1,R_##W##x4_1_1); X1 ^= X2; \
     * }                                                                   \ *)
    if nrounds>lbound then Num.(
        x0 := add !x0 !x3; x3 := rotL !x3 i_0; x3 := logxor !x3 !x0;
        x2 := add !x2 !x1; x1 := rotL !x1 i_1; x1 := logxor !x1 !x2;
      )
    else ()

  let aux3 ~of_int lbound nrounds x0 x1 x2 x3 y0 y1 y2 y3 i =
    (* if(Nrounds>3){                                                      \
     *     /* InjectKey(r=1) */                                            \
     *     X0 += ks1; X1 += ks2; X2 += ks3; X3 += ks4; \
     *     X3 += 1;     /* XWCNT4-1 += r  */                 \
     * }                                                                   \ *)
    if nrounds>lbound then Num.(
        x0 := add !x0 y0; x1 := add !x1 y1; x2 := add !x2 y2; x3 := add !x3 y3;
        x3 := add !x3 (of_int i);
      )
    else ()

  let rand_R ~of_int ~rounds:nrounds ~key ~ctr =
    let open Num in
    let max_rounds = 72 in
    let _ = if nrounds > max_rounds then
        failwith @@ Printf.sprintf
          "number rounds must be <= %d" max_rounds
      else ()
    in
    let ks4 = skein_ks_parity in
    let ks0 = key.(0) in
    let x0 = ref (add ctr.(0) ks0) in
    let ks4 = logxor ks4 ks0 in

    let ks1 = key.(1) in
    let x1 = ref (add ctr.(1) ks1) in
    let ks4 = logxor ks4 ks1 in

    let ks2 = key.(2) in
    let x2 = ref (add ctr.(2) ks2) in
    let ks4 = logxor ks4 ks2 in

    let ks3 = key.(3) in
    let x3 = ref (add ctr.(3) ks3) in
    let ks4 = logxor ks4 ks3 in

    let _aux1 = aux1 in
    let _aux2 = aux2 in
    let _aux3 = aux3 ~of_int in

    let _ = _aux1  0 nrounds x0 x1 x2 x3 rotations_0.i_0 rotations_1.i_0 in
    let _ = _aux2  1 nrounds x0 x1 x2 x3 rotations_0.i_1 rotations_1.i_1 in
    let _ = _aux1  2 nrounds x0 x1 x2 x3 rotations_0.i_2 rotations_1.i_2 in
    let _ = _aux2  3 nrounds x0 x1 x2 x3 rotations_0.i_3 rotations_1.i_3 in
    let _ = _aux3  3 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 1 in

    let _ = _aux1  4 nrounds x0 x1 x2 x3 rotations_0.i_4 rotations_1.i_4 in
    let _ = _aux2  5 nrounds x0 x1 x2 x3 rotations_0.i_5 rotations_1.i_5 in
    let _ = _aux1  6 nrounds x0 x1 x2 x3 rotations_0.i_6 rotations_1.i_6 in
    let _ = _aux2  7 nrounds x0 x1 x2 x3 rotations_0.i_7 rotations_1.i_7 in
    let _ = _aux3  7 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 2 in

    let _ = _aux1  8 nrounds x0 x1 x2 x3 rotations_0.i_0 rotations_1.i_0 in
    let _ = _aux2  9 nrounds x0 x1 x2 x3 rotations_0.i_1 rotations_1.i_1 in
    let _ = _aux1 10 nrounds x0 x1 x2 x3 rotations_0.i_2 rotations_1.i_2 in
    let _ = _aux2 11 nrounds x0 x1 x2 x3 rotations_0.i_3 rotations_1.i_3 in
    let _ = _aux3 11 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 3 in

    let _ = _aux1 12 nrounds x0 x1 x2 x3 rotations_0.i_4 rotations_1.i_4 in
    let _ = _aux2 13 nrounds x0 x1 x2 x3 rotations_0.i_5 rotations_1.i_5 in
    let _ = _aux1 14 nrounds x0 x1 x2 x3 rotations_0.i_6 rotations_1.i_6 in
    let _ = _aux2 15 nrounds x0 x1 x2 x3 rotations_0.i_7 rotations_1.i_7 in
    let _ = _aux3 15 nrounds x0 x1 x2 x3 ks4 ks0 ks1 ks2 4 in

    let _ = _aux1 16 nrounds x0 x1 x2 x3 rotations_0.i_0 rotations_1.i_0 in
    let _ = _aux2 17 nrounds x0 x1 x2 x3 rotations_0.i_1 rotations_1.i_1 in
    let _ = _aux1 18 nrounds x0 x1 x2 x3 rotations_0.i_2 rotations_1.i_2 in
    let _ = _aux2 19 nrounds x0 x1 x2 x3 rotations_0.i_3 rotations_1.i_3 in
    let _ = _aux3 19 nrounds x0 x1 x2 x3 ks0 ks1 ks2 ks3 5 in

    let _ = _aux1 20 nrounds x0 x1 x2 x3 rotations_0.i_4 rotations_1.i_4 in
    let _ = _aux2 21 nrounds x0 x1 x2 x3 rotations_0.i_5 rotations_1.i_5 in
    let _ = _aux1 22 nrounds x0 x1 x2 x3 rotations_0.i_6 rotations_1.i_6 in
    let _ = _aux2 23 nrounds x0 x1 x2 x3 rotations_0.i_7 rotations_1.i_7 in
    let _ = _aux3 23 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 6 in

    let _ = _aux1 24 nrounds x0 x1 x2 x3 rotations_0.i_0 rotations_1.i_0 in
    let _ = _aux2 25 nrounds x0 x1 x2 x3 rotations_0.i_1 rotations_1.i_1 in
    let _ = _aux1 26 nrounds x0 x1 x2 x3 rotations_0.i_2 rotations_1.i_2 in
    let _ = _aux2 27 nrounds x0 x1 x2 x3 rotations_0.i_3 rotations_1.i_3 in
    let _ = _aux3 27 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 7 in

    let _ = _aux1 28 nrounds x0 x1 x2 x3 rotations_0.i_4 rotations_1.i_4 in
    let _ = _aux2 29 nrounds x0 x1 x2 x3 rotations_0.i_5 rotations_1.i_5 in
    let _ = _aux1 30 nrounds x0 x1 x2 x3 rotations_0.i_6 rotations_1.i_6 in
    let _ = _aux2 31 nrounds x0 x1 x2 x3 rotations_0.i_7 rotations_1.i_7 in
    let _ = _aux3 31 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 8 in

    let _ = _aux1 32 nrounds x0 x1 x2 x3 rotations_0.i_0 rotations_1.i_0 in
    let _ = _aux2 33 nrounds x0 x1 x2 x3 rotations_0.i_1 rotations_1.i_1 in
    let _ = _aux1 34 nrounds x0 x1 x2 x3 rotations_0.i_2 rotations_1.i_2 in
    let _ = _aux2 35 nrounds x0 x1 x2 x3 rotations_0.i_3 rotations_1.i_3 in
    let _ = _aux3 35 nrounds x0 x1 x2 x3 ks4 ks0 ks1 ks2 9 in

    let _ = _aux1 36 nrounds x0 x1 x2 x3 rotations_0.i_4 rotations_1.i_4 in
    let _ = _aux2 37 nrounds x0 x1 x2 x3 rotations_0.i_5 rotations_1.i_5 in
    let _ = _aux1 38 nrounds x0 x1 x2 x3 rotations_0.i_6 rotations_1.i_6 in
    let _ = _aux2 39 nrounds x0 x1 x2 x3 rotations_0.i_7 rotations_1.i_7 in
    let _ = _aux3 39 nrounds x0 x1 x2 x3 ks0 ks1 ks2 ks3 10 in

    let _ = _aux1 40 nrounds x0 x1 x2 x3 rotations_0.i_0 rotations_1.i_0 in
    let _ = _aux2 41 nrounds x0 x1 x2 x3 rotations_0.i_1 rotations_1.i_1 in
    let _ = _aux1 42 nrounds x0 x1 x2 x3 rotations_0.i_2 rotations_1.i_2 in
    let _ = _aux2 43 nrounds x0 x1 x2 x3 rotations_0.i_3 rotations_1.i_3 in
    let _ = _aux3 43 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 11 in

    let _ = _aux1 44 nrounds x0 x1 x2 x3 rotations_0.i_4 rotations_1.i_4 in
    let _ = _aux2 45 nrounds x0 x1 x2 x3 rotations_0.i_5 rotations_1.i_5 in
    let _ = _aux1 46 nrounds x0 x1 x2 x3 rotations_0.i_6 rotations_1.i_6 in
    let _ = _aux2 47 nrounds x0 x1 x2 x3 rotations_0.i_7 rotations_1.i_7 in
    let _ = _aux3 47 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 12 in

    let _ = _aux1 48 nrounds x0 x1 x2 x3 rotations_0.i_0 rotations_1.i_0 in
    let _ = _aux2 49 nrounds x0 x1 x2 x3 rotations_0.i_1 rotations_1.i_1 in
    let _ = _aux1 50 nrounds x0 x1 x2 x3 rotations_0.i_2 rotations_1.i_2 in
    let _ = _aux2 51 nrounds x0 x1 x2 x3 rotations_0.i_3 rotations_1.i_3 in
    let _ = _aux3 51 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 13 in

    let _ = _aux1 52 nrounds x0 x1 x2 x3 rotations_0.i_4 rotations_1.i_4 in
    let _ = _aux2 53 nrounds x0 x1 x2 x3 rotations_0.i_5 rotations_1.i_5 in
    let _ = _aux1 54 nrounds x0 x1 x2 x3 rotations_0.i_6 rotations_1.i_6 in
    let _ = _aux2 55 nrounds x0 x1 x2 x3 rotations_0.i_7 rotations_1.i_7 in
    let _ = _aux3 55 nrounds x0 x1 x2 x3 ks4 ks0 ks1 ks2 14 in

    let _ = _aux1 56 nrounds x0 x1 x2 x3 rotations_0.i_0 rotations_1.i_0 in
    let _ = _aux2 57 nrounds x0 x1 x2 x3 rotations_0.i_1 rotations_1.i_1 in
    let _ = _aux1 58 nrounds x0 x1 x2 x3 rotations_0.i_2 rotations_1.i_2 in
    let _ = _aux2 59 nrounds x0 x1 x2 x3 rotations_0.i_3 rotations_1.i_3 in
    let _ = _aux3 59 nrounds x0 x1 x2 x3 ks0 ks1 ks2 ks3 15 in

    let _ = _aux1 60 nrounds x0 x1 x2 x3 rotations_0.i_4 rotations_1.i_4 in
    let _ = _aux2 61 nrounds x0 x1 x2 x3 rotations_0.i_5 rotations_1.i_5 in
    let _ = _aux1 62 nrounds x0 x1 x2 x3 rotations_0.i_6 rotations_1.i_6 in
    let _ = _aux2 63 nrounds x0 x1 x2 x3 rotations_0.i_7 rotations_1.i_7 in
    let _ = _aux3 63 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 16 in

    let _ = _aux1 64 nrounds x0 x1 x2 x3 rotations_0.i_0 rotations_1.i_0 in
    let _ = _aux2 65 nrounds x0 x1 x2 x3 rotations_0.i_1 rotations_1.i_1 in
    let _ = _aux1 66 nrounds x0 x1 x2 x3 rotations_0.i_2 rotations_1.i_2 in
    let _ = _aux2 67 nrounds x0 x1 x2 x3 rotations_0.i_3 rotations_1.i_3 in
    let _ = _aux3 67 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 17 in

    let _ = _aux1 68 nrounds x0 x1 x2 x3 rotations_0.i_4 rotations_1.i_4 in
    let _ = _aux2 69 nrounds x0 x1 x2 x3 rotations_0.i_5 rotations_1.i_5 in
    let _ = _aux1 70 nrounds x0 x1 x2 x3 rotations_0.i_6 rotations_1.i_6 in
    let _ = _aux2 71 nrounds x0 x1 x2 x3 rotations_0.i_7 rotations_1.i_7 in
    let _ = _aux3 71 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 18 in

    [|!x0; !x1; !x2; !x3|]

end

let default_rounds = 20

module type RNG = sig
  type t
  val of_int_array : int array -> t
  val to_int_array : t -> int array
  val of_string_array : string array -> t
  val to_string_array : t -> string array
  val rand : ?rounds:int -> key:t -> ctr:t -> unit -> t
  val succ : t -> t
end

module Threefry_2_32 (Num:NUM) : RNG = struct
  module T = Threefry_2_XX(Num)

  type t = (digits_two, word_32) Num.t array

  let of_int_array arr = assert (2 == Array.length arr); Array.map Num.of_int_2_32 arr
  let to_int_array = Array.map Num.to_int
  let of_string_array arr = assert (2 == Array.length arr); Array.map Num.of_string_2_32 arr
  let to_string_array = Array.map Num.to_string

  let rand ?(rounds=default_rounds) ~key ~ctr () =
    T.rand_R ~of_int:Num.of_int_2_32 ~rounds ~key ~ctr

  let succ t = ctr ~equal:Num.equal 0 Num.succ Num.zero t; t
end

module Threefry_4_32 (Num:NUM) : RNG = struct
  module T = Threefry_4_XX(Num)

  type t = (digits_four, word_32) Num.t array

  let of_int_array arr = assert (4 == Array.length arr); Array.map Num.of_int_4_32 arr
  let to_int_array = Array.map Num.to_int
  let of_string_array arr = assert (4 == Array.length arr); Array.map Num.of_string_4_32 arr
  let to_string_array = Array.map Num.to_string

  let rand ?(rounds=default_rounds) ~key ~ctr () =
    T.rand_R ~of_int:Num.of_int_4_32 ~rounds ~key ~ctr

  let succ t = ctr ~equal:Num.equal 0 Num.succ Num.zero t; t
end

module Threefry_2_64 (Num:NUM) : RNG = struct
  module T = Threefry_2_XX(Num)

  type t = (digits_two, word_64) Num.t array

  let of_int_array arr = assert (2 == Array.length arr); Array.map Num.of_int_2_64 arr
  let to_int_array = Array.map Num.to_int
  let of_string_array arr = assert (2 == Array.length arr); Array.map Num.of_string_2_64 arr
  let to_string_array = Array.map Num.to_string

  let rand ?(rounds=default_rounds) ~key ~ctr () =
    T.rand_R ~of_int:Num.of_int_2_64 ~rounds ~key ~ctr

  let succ t = ctr ~equal:Num.equal 0 Num.succ Num.zero t; t
end

module Threefry_4_64 (Num:NUM) : RNG = struct
  module T = Threefry_4_XX(Num)

  type t = (digits_four, word_64) Num.t array

  let of_int_array arr = assert (4 == Array.length arr); Array.map Num.of_int_4_64 arr
  let to_int_array = Array.map Num.to_int
  let of_string_array arr = assert (4 == Array.length arr); Array.map Num.of_string_4_64 arr
  let to_string_array = Array.map Num.to_string

  let rand ?(rounds=default_rounds) ~key ~ctr () =
    T.rand_R ~of_int:Num.of_int_4_64 ~rounds ~key ~ctr

  let succ t = ctr ~equal:Num.equal 0 Num.succ Num.zero t; t
end
