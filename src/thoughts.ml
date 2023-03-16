type digits_two
type digits_four
type word_32
type word_64


(* factor out dependence on UInt32 / UInt64 and word size (32/64 bit) *)
module type NUM = sig

  type digits
  type word
  type ('digits, 'word) t

  (* ctors *)
  val of_int : int -> (digits, word) t
  val of_string : string -> (digits, word) t
  val zero : (digits, word) t
  val one : (digits, word) t
  val max_int : (digits, word) t

  val skein_ks_parity : (digits, word) t
  val rotations_0 : (digits, word) t Types.Consts.t
  val rotations_1 : (digits, word) t Types.Consts.t

  (* dtors *)
  val to_int : ('digits, 'word) t -> int
  val to_string : ('digits, 'word) t -> string

  val digits : int
  val equal : ('digits, 'word) t -> ('digits, 'word) t -> bool
  val succ : ('digits, 'word) t -> ('digits, 'word) t
  val pred : ('digits, 'word) t -> ('digits, 'word) t
  val add : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t
  val sub : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t
  val rem : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t
  val logxor : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t
  val rotL : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t
end

module Num_uint32_2 = struct
  type digits = digits_two
  type word = word_32
  type (_,_) t = Unsigned.UInt32.t
  module U = Unsigned.UInt32

  let of_int = U.of_int
  let of_string = U.of_string
  let zero = of_int 0
  let one = of_int 1
  let max_int = U.max_int

  let to_int = U.to_int
  let to_string = U.to_string

  let digits = 2
  let equal = U.equal
  let succ = U.succ
  let pred = U.pred
  let add = U.add
  let sub = U.sub
  let rem = U.rem
  let logxor = U.logxor

  (* #define SKEIN_KS_PARITY32         0x1BD11BDA *)
  let skein_ks_parity = "0x1BD11BDA" |> of_string

  let rotations_0 = Types.Consts.make
      of_int
      13
      15
      26
      6
      17
      29
      16
      24

  let rotations_1 = Types.Consts.zeros of_int

  let _31 = 31 |> of_int
  let _32 = 32 |> of_int

  let rotL x n =
    let l = U.logand n _31 |> to_int in
    let left = U.shift_left x l in
    let r = (U.logand (U.sub _32 n) _31) |> to_int in
    let right = U.shift_right x r in
    U.logor left right

end

module Num_uint32_4 = struct
  include Num_uint32_2
  type digits = digits_four
  type word = word_32

  let digits = 4
  let rotations_0 = Types.Consts.make
      of_int
      10
      11
      13
      23
      6
      17
      25
      18

  let rotations_1 = Types.Consts.make
      of_int
      26
      21
      27
      5
      20
      11
      10
      20
end


module Num_uint64_2 = struct
  type (_,_) t = Unsigned.UInt64.t
  type digits = digits_two
  type word = word_64
  module U = Unsigned.UInt64

  let of_int = U.of_int
  let to_int = U.to_int

  let of_string = U.of_string
  let to_string = U.to_string

  let of_int = U.of_int
  let of_string = U.of_string
  let zero = of_int 0
  let one = of_int 1
  let max_int = U.max_int

  let to_int = U.to_int
  let to_string = U.to_string

  let digits = 2
  let equal = U.equal
  let succ = U.succ
  let pred = U.pred
  let add = U.add
  let sub = U.sub
  let rem = U.rem
  let logxor = U.logxor

  (* #define SKEIN_MK_64(hi32,lo32)  ((lo32) + (((uint64_t) (hi32)) << 32))
   * #define SKEIN_KS_PARITY64         SKEIN_MK_64(0x1BD11BDA,0xA9FC1A22) *)
  let skein_ks_parity =
    let hi = "0x1BD11BDA" |> of_string in
    let lo = "0xA9FC1A22" |> of_string in
    add lo (U.shift_left hi 32)

  let rotations_0 = Types.Consts.make
      of_int
      16
      42
      12
      31
      16
      32
      24
      21

  let rotations_1 = Types.Consts.zeros of_int

  let _63 = 63 |> of_int
  let _64 = 64 |> of_int

  let rotL x n =
    let l = U.logand n _63 |> to_int in
    let left = U.shift_left x l in
    let r = (U.logand (U.sub _64 n) _63) |> to_int in
    let right = U.shift_right x r in
    U.logor left right

end

module Num_uint64_4 = struct
  include Num_uint64_2
  type digits = digits_four
  type word = word_64

  let digits = 4
  let rotations_0 = Types.Consts.make
      of_int
      14
      52
      23
      5
      25
      46
      58
      32

  let rotations_1 = Types.Consts.make
      of_int
      16
      57
      40
      37
      33
      12
      22
      32

end

module type RNG = sig
  type digits
  module Make : functor (Num:NUM) -> sig
    val rand_R :
      of_int:(int -> (Num.digits, Num.word) Num.t) ->
      rounds:int ->
      key:(Num.digits, Num.word) Num.t array ->
      ctr:(Num.digits, Num.word) Num.t array ->
      (Num.digits, Num.word) Num.t array
  end
end

module Make_threefry_2_XX = struct
  type digits = digits_two
  module Make (Num:NUM) = struct

    let aux1 lbound nrounds x0 x1 i =
      (* if(Nrounds>0){  X0 += X1; X1 = RotL_##W(X1,R_##W##x2_0_0); X1 ^= X0; }  *)
      if nrounds>lbound then (
        x0 := Num.add !x0 !x1; x1 := Num.rotL !x1 i; x1 := Num.logxor !x1 !x0;
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
      if nrounds>lbound then (
        x0 := Num.add !x0 ks0; x1 := Num.add !x1 ks1;
        x1 := Num.add !x1 (of_int i)
      )
      else ()

    let rand_R ~of_int ~rounds:nrounds ~key ~ctr =
      let max_rounds = 32 in
      let _ = if nrounds > max_rounds then
          failwith @@ Printf.sprintf
            "number rounds must be <= %d" max_rounds
        else ()
      in
      let ks2 = Num.skein_ks_parity in
      let ks0 = key.(0) in
      let x0 = ref (Num.add ctr.(0) ks0) in
      let ks2 = Num.logxor ks2 ks0 in

      let ks1 = key.(1) in
      let x1 = ref (Num.add ctr.(1) ks1) in
      let ks2 = Num.logxor ks2 ks1 in

      let _aux1 = aux1 in
      let _aux3 = aux3 ~of_int in

      let _ = _aux1  0 nrounds x0 x1 Num.rotations_0.i_0 in
      let _ = _aux1  1 nrounds x0 x1 Num.rotations_0.i_1 in
      let _ = _aux1  2 nrounds x0 x1 Num.rotations_0.i_2 in
      let _ = _aux1  3 nrounds x0 x1 Num.rotations_0.i_3 in
      let _ = _aux3  3 nrounds x0 x1 ks1 ks2 1 in

      let _ = _aux1  4 nrounds x0 x1 Num.rotations_0.i_4 in
      let _ = _aux1  5 nrounds x0 x1 Num.rotations_0.i_5 in
      let _ = _aux1  6 nrounds x0 x1 Num.rotations_0.i_6 in
      let _ = _aux1  7 nrounds x0 x1 Num.rotations_0.i_7 in
      let _ = _aux3  7 nrounds x0 x1 ks2 ks0 2 in

      let _ = _aux1  8 nrounds x0 x1 Num.rotations_0.i_0 in
      let _ = _aux1  9 nrounds x0 x1 Num.rotations_0.i_1 in
      let _ = _aux1 10 nrounds x0 x1 Num.rotations_0.i_2 in
      let _ = _aux1 11 nrounds x0 x1 Num.rotations_0.i_3 in
      let _ = _aux3 11 nrounds x0 x1 ks0 ks1 3 in

      let _ = _aux1 12 nrounds x0 x1 Num.rotations_0.i_4 in
      let _ = _aux1 13 nrounds x0 x1 Num.rotations_0.i_5 in
      let _ = _aux1 14 nrounds x0 x1 Num.rotations_0.i_6 in
      let _ = _aux1 15 nrounds x0 x1 Num.rotations_0.i_7 in
      let _ = _aux3 15 nrounds x0 x1 ks1 ks2 4 in

      let _ = _aux1 16 nrounds x0 x1 Num.rotations_0.i_0 in
      let _ = _aux1 17 nrounds x0 x1 Num.rotations_0.i_1 in
      let _ = _aux1 18 nrounds x0 x1 Num.rotations_0.i_2 in
      let _ = _aux1 19 nrounds x0 x1 Num.rotations_0.i_3 in
      let _ = _aux3 19 nrounds x0 x1 ks2 ks0 5 in

      let _ = _aux1 20 nrounds x0 x1 Num.rotations_0.i_4 in
      let _ = _aux1 21 nrounds x0 x1 Num.rotations_0.i_5 in
      let _ = _aux1 22 nrounds x0 x1 Num.rotations_0.i_6 in
      let _ = _aux1 23 nrounds x0 x1 Num.rotations_0.i_7 in
      let _ = _aux3 23 nrounds x0 x1 ks0 ks1 6 in

      let _ = _aux1 24 nrounds x0 x1 Num.rotations_0.i_0 in
      let _ = _aux1 25 nrounds x0 x1 Num.rotations_0.i_1 in
      let _ = _aux1 26 nrounds x0 x1 Num.rotations_0.i_2 in
      let _ = _aux1 27 nrounds x0 x1 Num.rotations_0.i_3 in
      let _ = _aux3 27 nrounds x0 x1 ks1 ks2 7 in

      let _ = _aux1 28 nrounds x0 x1 Num.rotations_0.i_4 in
      let _ = _aux1 29 nrounds x0 x1 Num.rotations_0.i_5 in
      let _ = _aux1 30 nrounds x0 x1 Num.rotations_0.i_6 in
      let _ = _aux1 31 nrounds x0 x1 Num.rotations_0.i_7 in
      let _ = _aux3 31 nrounds x0 x1 ks2 ks0 8 in

      [|!x0; !x1|]
  end
end

module Make_threefry_4_XX = struct
  type digits = digits_four
  module Make (Num:NUM) = struct

    let aux1 lbound nrounds x0 x1 x2 x3 i_0 i_1 =
      (* if(Nrounds>0){                                                      \
       *     X0 += X1; X1 = RotL_##W(X1,R_##W##x4_0_0); X1 ^= X0; \
       *     X2 += X3; X3 = RotL_##W(X3,R_##W##x4_0_1); X3 ^= X2; \
       * }                                                                   \ *)
      if nrounds>lbound then (
        x0 := Num.add !x0 !x1; x1 := Num.rotL !x1 i_0; x1 := Num.logxor !x1 !x0;
        x2 := Num.add !x2 !x3; x3 := Num.rotL !x3 i_1; x3 := Num.logxor !x3 !x2;
      )
      else ()

    let aux2 lbound nrounds x0 x1 x2 x3 i_0 i_1 =
      (* if(Nrounds>1){                                                      \
       *     X0 += X3; X3 = RotL_##W(X3,R_##W##x4_1_0); X3 ^= X0; \
       *     X2 += X1; X1 = RotL_##W(X1,R_##W##x4_1_1); X1 ^= X2; \
       * }                                                                   \ *)
      if nrounds>lbound then (
        x0 := Num.add !x0 !x3; x3 := Num.rotL !x3 i_0; x3 := Num.logxor !x3 !x0;
        x2 := Num.add !x2 !x1; x1 := Num.rotL !x1 i_1; x1 := Num.logxor !x1 !x2;
      )
      else ()

    let aux3 ~of_int lbound nrounds x0 x1 x2 x3 y0 y1 y2 y3 i =
      (* if(Nrounds>3){                                                      \
       *     /* InjectKey(r=1) */                                            \
       *     X0 += ks1; X1 += ks2; X2 += ks3; X3 += ks4; \
       *     X3 += 1;     /* XWCNT4-1 += r  */                 \
       * }                                                                   \ *)
      if nrounds>lbound then (
        x0 := Num.add !x0 y0; x1 := Num.add !x1 y1; x2 := Num.add !x2 y2; x3 := Num.add !x3 y3;
        x3 := Num.add !x3 (of_int i);
      )
      else ()

    let rand_R ~of_int ~rounds:nrounds ~key ~ctr =
      let max_rounds = 72 in
      let _ = if nrounds > max_rounds then
          failwith @@ Printf.sprintf
            "number rounds must be <= %d" max_rounds
        else ()
      in
      let ks4 = Num.skein_ks_parity in
      let ks0 = key.(0) in
      let x0 = ref (Num.add ctr.(0) ks0) in
      let ks4 = Num.logxor ks4 ks0 in

      let ks1 = key.(1) in
      let x1 = ref (Num.add ctr.(1) ks1) in
      let ks4 = Num.logxor ks4 ks1 in

      let ks2 = key.(2) in
      let x2 = ref (Num.add ctr.(2) ks2) in
      let ks4 = Num.logxor ks4 ks2 in

      let ks3 = key.(3) in
      let x3 = ref (Num.add ctr.(3) ks3) in
      let ks4 = Num.logxor ks4 ks3 in

      let _aux1 = aux1 in
      let _aux2 = aux2 in
      let _aux3 = aux3 ~of_int in

      let _ = _aux1  0 nrounds x0 x1 x2 x3 Num.rotations_0.i_0 Num.rotations_1.i_0 in
      let _ = _aux2  1 nrounds x0 x1 x2 x3 Num.rotations_0.i_1 Num.rotations_1.i_1 in
      let _ = _aux1  2 nrounds x0 x1 x2 x3 Num.rotations_0.i_2 Num.rotations_1.i_2 in
      let _ = _aux2  3 nrounds x0 x1 x2 x3 Num.rotations_0.i_3 Num.rotations_1.i_3 in
      let _ = _aux3  3 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 1 in

      let _ = _aux1  4 nrounds x0 x1 x2 x3 Num.rotations_0.i_4 Num.rotations_1.i_4 in
      let _ = _aux2  5 nrounds x0 x1 x2 x3 Num.rotations_0.i_5 Num.rotations_1.i_5 in
      let _ = _aux1  6 nrounds x0 x1 x2 x3 Num.rotations_0.i_6 Num.rotations_1.i_6 in
      let _ = _aux2  7 nrounds x0 x1 x2 x3 Num.rotations_0.i_7 Num.rotations_1.i_7 in
      let _ = _aux3  7 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 2 in

      let _ = _aux1  8 nrounds x0 x1 x2 x3 Num.rotations_0.i_0 Num.rotations_1.i_0 in
      let _ = _aux2  9 nrounds x0 x1 x2 x3 Num.rotations_0.i_1 Num.rotations_1.i_1 in
      let _ = _aux1 10 nrounds x0 x1 x2 x3 Num.rotations_0.i_2 Num.rotations_1.i_2 in
      let _ = _aux2 11 nrounds x0 x1 x2 x3 Num.rotations_0.i_3 Num.rotations_1.i_3 in
      let _ = _aux3 11 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 3 in

      let _ = _aux1 12 nrounds x0 x1 x2 x3 Num.rotations_0.i_4 Num.rotations_1.i_4 in
      let _ = _aux2 13 nrounds x0 x1 x2 x3 Num.rotations_0.i_5 Num.rotations_1.i_5 in
      let _ = _aux1 14 nrounds x0 x1 x2 x3 Num.rotations_0.i_6 Num.rotations_1.i_6 in
      let _ = _aux2 15 nrounds x0 x1 x2 x3 Num.rotations_0.i_7 Num.rotations_1.i_7 in
      let _ = _aux3 15 nrounds x0 x1 x2 x3 ks4 ks0 ks1 ks2 4 in

      let _ = _aux1 16 nrounds x0 x1 x2 x3 Num.rotations_0.i_0 Num.rotations_1.i_0 in
      let _ = _aux2 17 nrounds x0 x1 x2 x3 Num.rotations_0.i_1 Num.rotations_1.i_1 in
      let _ = _aux1 18 nrounds x0 x1 x2 x3 Num.rotations_0.i_2 Num.rotations_1.i_2 in
      let _ = _aux2 19 nrounds x0 x1 x2 x3 Num.rotations_0.i_3 Num.rotations_1.i_3 in
      let _ = _aux3 19 nrounds x0 x1 x2 x3 ks0 ks1 ks2 ks3 5 in

      let _ = _aux1 20 nrounds x0 x1 x2 x3 Num.rotations_0.i_4 Num.rotations_1.i_4 in
      let _ = _aux2 21 nrounds x0 x1 x2 x3 Num.rotations_0.i_5 Num.rotations_1.i_5 in
      let _ = _aux1 22 nrounds x0 x1 x2 x3 Num.rotations_0.i_6 Num.rotations_1.i_6 in
      let _ = _aux2 23 nrounds x0 x1 x2 x3 Num.rotations_0.i_7 Num.rotations_1.i_7 in
      let _ = _aux3 23 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 6 in

      let _ = _aux1 24 nrounds x0 x1 x2 x3 Num.rotations_0.i_0 Num.rotations_1.i_0 in
      let _ = _aux2 25 nrounds x0 x1 x2 x3 Num.rotations_0.i_1 Num.rotations_1.i_1 in
      let _ = _aux1 26 nrounds x0 x1 x2 x3 Num.rotations_0.i_2 Num.rotations_1.i_2 in
      let _ = _aux2 27 nrounds x0 x1 x2 x3 Num.rotations_0.i_3 Num.rotations_1.i_3 in
      let _ = _aux3 27 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 7 in

      let _ = _aux1 28 nrounds x0 x1 x2 x3 Num.rotations_0.i_4 Num.rotations_1.i_4 in
      let _ = _aux2 29 nrounds x0 x1 x2 x3 Num.rotations_0.i_5 Num.rotations_1.i_5 in
      let _ = _aux1 30 nrounds x0 x1 x2 x3 Num.rotations_0.i_6 Num.rotations_1.i_6 in
      let _ = _aux2 31 nrounds x0 x1 x2 x3 Num.rotations_0.i_7 Num.rotations_1.i_7 in
      let _ = _aux3 31 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 8 in

      let _ = _aux1 32 nrounds x0 x1 x2 x3 Num.rotations_0.i_0 Num.rotations_1.i_0 in
      let _ = _aux2 33 nrounds x0 x1 x2 x3 Num.rotations_0.i_1 Num.rotations_1.i_1 in
      let _ = _aux1 34 nrounds x0 x1 x2 x3 Num.rotations_0.i_2 Num.rotations_1.i_2 in
      let _ = _aux2 35 nrounds x0 x1 x2 x3 Num.rotations_0.i_3 Num.rotations_1.i_3 in
      let _ = _aux3 35 nrounds x0 x1 x2 x3 ks4 ks0 ks1 ks2 9 in

      let _ = _aux1 36 nrounds x0 x1 x2 x3 Num.rotations_0.i_4 Num.rotations_1.i_4 in
      let _ = _aux2 37 nrounds x0 x1 x2 x3 Num.rotations_0.i_5 Num.rotations_1.i_5 in
      let _ = _aux1 38 nrounds x0 x1 x2 x3 Num.rotations_0.i_6 Num.rotations_1.i_6 in
      let _ = _aux2 39 nrounds x0 x1 x2 x3 Num.rotations_0.i_7 Num.rotations_1.i_7 in
      let _ = _aux3 39 nrounds x0 x1 x2 x3 ks0 ks1 ks2 ks3 10 in

      let _ = _aux1 40 nrounds x0 x1 x2 x3 Num.rotations_0.i_0 Num.rotations_1.i_0 in
      let _ = _aux2 41 nrounds x0 x1 x2 x3 Num.rotations_0.i_1 Num.rotations_1.i_1 in
      let _ = _aux1 42 nrounds x0 x1 x2 x3 Num.rotations_0.i_2 Num.rotations_1.i_2 in
      let _ = _aux2 43 nrounds x0 x1 x2 x3 Num.rotations_0.i_3 Num.rotations_1.i_3 in
      let _ = _aux3 43 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 11 in

      let _ = _aux1 44 nrounds x0 x1 x2 x3 Num.rotations_0.i_4 Num.rotations_1.i_4 in
      let _ = _aux2 45 nrounds x0 x1 x2 x3 Num.rotations_0.i_5 Num.rotations_1.i_5 in
      let _ = _aux1 46 nrounds x0 x1 x2 x3 Num.rotations_0.i_6 Num.rotations_1.i_6 in
      let _ = _aux2 47 nrounds x0 x1 x2 x3 Num.rotations_0.i_7 Num.rotations_1.i_7 in
      let _ = _aux3 47 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 12 in

      let _ = _aux1 48 nrounds x0 x1 x2 x3 Num.rotations_0.i_0 Num.rotations_1.i_0 in
      let _ = _aux2 49 nrounds x0 x1 x2 x3 Num.rotations_0.i_1 Num.rotations_1.i_1 in
      let _ = _aux1 50 nrounds x0 x1 x2 x3 Num.rotations_0.i_2 Num.rotations_1.i_2 in
      let _ = _aux2 51 nrounds x0 x1 x2 x3 Num.rotations_0.i_3 Num.rotations_1.i_3 in
      let _ = _aux3 51 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 13 in

      let _ = _aux1 52 nrounds x0 x1 x2 x3 Num.rotations_0.i_4 Num.rotations_1.i_4 in
      let _ = _aux2 53 nrounds x0 x1 x2 x3 Num.rotations_0.i_5 Num.rotations_1.i_5 in
      let _ = _aux1 54 nrounds x0 x1 x2 x3 Num.rotations_0.i_6 Num.rotations_1.i_6 in
      let _ = _aux2 55 nrounds x0 x1 x2 x3 Num.rotations_0.i_7 Num.rotations_1.i_7 in
      let _ = _aux3 55 nrounds x0 x1 x2 x3 ks4 ks0 ks1 ks2 14 in

      let _ = _aux1 56 nrounds x0 x1 x2 x3 Num.rotations_0.i_0 Num.rotations_1.i_0 in
      let _ = _aux2 57 nrounds x0 x1 x2 x3 Num.rotations_0.i_1 Num.rotations_1.i_1 in
      let _ = _aux1 58 nrounds x0 x1 x2 x3 Num.rotations_0.i_2 Num.rotations_1.i_2 in
      let _ = _aux2 59 nrounds x0 x1 x2 x3 Num.rotations_0.i_3 Num.rotations_1.i_3 in
      let _ = _aux3 59 nrounds x0 x1 x2 x3 ks0 ks1 ks2 ks3 15 in

      let _ = _aux1 60 nrounds x0 x1 x2 x3 Num.rotations_0.i_4 Num.rotations_1.i_4 in
      let _ = _aux2 61 nrounds x0 x1 x2 x3 Num.rotations_0.i_5 Num.rotations_1.i_5 in
      let _ = _aux1 62 nrounds x0 x1 x2 x3 Num.rotations_0.i_6 Num.rotations_1.i_6 in
      let _ = _aux2 63 nrounds x0 x1 x2 x3 Num.rotations_0.i_7 Num.rotations_1.i_7 in
      let _ = _aux3 63 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 16 in

      let _ = _aux1 64 nrounds x0 x1 x2 x3 Num.rotations_0.i_0 Num.rotations_1.i_0 in
      let _ = _aux2 65 nrounds x0 x1 x2 x3 Num.rotations_0.i_1 Num.rotations_1.i_1 in
      let _ = _aux1 66 nrounds x0 x1 x2 x3 Num.rotations_0.i_2 Num.rotations_1.i_2 in
      let _ = _aux2 67 nrounds x0 x1 x2 x3 Num.rotations_0.i_3 Num.rotations_1.i_3 in
      let _ = _aux3 67 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 17 in

      let _ = _aux1 68 nrounds x0 x1 x2 x3 Num.rotations_0.i_4 Num.rotations_1.i_4 in
      let _ = _aux2 69 nrounds x0 x1 x2 x3 Num.rotations_0.i_5 Num.rotations_1.i_5 in
      let _ = _aux1 70 nrounds x0 x1 x2 x3 Num.rotations_0.i_6 Num.rotations_1.i_6 in
      let _ = _aux2 71 nrounds x0 x1 x2 x3 Num.rotations_0.i_7 Num.rotations_1.i_7 in
      let _ = _aux3 71 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 18 in

      [|!x0; !x1; !x2; !x3|]

  end
end


let default_rounds = 20

module type CTR = sig
  type t
  val of_int_array : int array ->  t
  val to_int_array :  t -> int array
  val of_string_array : string array ->  t
  val to_string_array :  t -> string array
  val succ :  t ->  t
  val rand : ?rounds:int -> key:t -> ctr:t -> unit -> t
end

module Make_threefry (Num:NUM) (Rng:RNG with type digits = Num.digits) : CTR = struct

  module T = Rng.Make(Num)

  type t = (Num.digits, Num.word) Num.t array

  let of_int_array arr = assert (Num.digits == Array.length arr); Array.map Num.of_int arr
  let to_int_array = Array.map Num.to_int

  let of_string_array arr = assert (Num.digits == Array.length arr); Array.map Num.of_string arr
  let to_string_array = Array.map Num.to_string

  let rec ctr ~equal digit op sentinal t =
    let n = Array.length t in
    if digit == n then () else (
      let d = op t.(digit) in
      match (equal sentinal d) with
      | false -> t.(digit) <- d;
      | true -> t.(digit) <- d; ctr ~equal (digit+1) op sentinal t
    )

  let succ t = ctr ~equal:Num.equal 0 Num.succ Num.zero t; t

  let rand ?(rounds=default_rounds) ~key ~ctr () =
    T.rand_R ~of_int:Num.of_int ~rounds ~key ~ctr
end

module Threefry_2_32 = Make_threefry (Num_uint32_2) (Make_threefry_2_XX)
module Threefry_4_32 = Make_threefry (Num_uint32_4) (Make_threefry_4_XX)
module Threefry_2_64 = Make_threefry (Num_uint64_2) (Make_threefry_2_XX)
module Threefry_4_64 = Make_threefry (Num_uint64_4) (Make_threefry_4_XX)
