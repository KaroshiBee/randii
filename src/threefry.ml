module Consts = struct
  type 'a t = {
    (* rotation amounts *)
    i_0: 'a;
    i_1: 'a;
    i_2: 'a;
    i_3: 'a;
    i_4: 'a;
    i_5: 'a;
    i_6: 'a;
    i_7: 'a;
  }

  let make i_0 i_1 i_2 i_3 i_4 i_5 i_6 i_7 = {
    i_0=i_0;
    i_1=i_1;
    i_2=i_2;
    i_3=i_3;
    i_4=i_4;
    i_5=i_5;
    i_6=i_6;
    i_7=i_7;
  }

end

module type T = sig
  type t
  val default_rounds : int
  val skein_ks_parity : t
  val rotations : t Consts.t
  val indices : t Consts.t
  val rotL : t -> t -> t

  val add : t -> t -> t
  val logxor : t -> t -> t

end

module ThreefryInt32 = struct

  type t = Int32.t

  let default_rounds = 20

  (* #define SKEIN_KS_PARITY32         0x1BD11BDA *)
  let skein_ks_parity = 0x1BD11BDA |> Int32.of_int

  let rotations = Consts.make
      13l
      15l
      26l
      6l
      17l
      29l
      16l
      24l

  let indices = Consts.make
      1l
      2l
      3l
      4l
      5l
      6l
      7l
      8l

  let rotL x n = Int32.(
      let l = logand n 31l |> to_int in
      let left = shift_left x l in
      let r = (logand (sub 32l n) 31l) |> to_int in
      let right = shift_right x r in
      logor left right
    )

  let add = Int32.add
  let logxor = Int32.logxor

end

module ThreefryInt64 = struct

  type t = Int64.t

  let default_rounds = 20

  let _skein_mk_64 hi32 lo32 = Int64.(
      (* #define SKEIN_MK_64(hi32,lo32)  ((lo32) + (((uint64_t) (hi32)) << 32))
       * #define SKEIN_KS_PARITY64         SKEIN_MK_64(0x1BD11BDA,0xA9FC1A22) *)
      (* convert hi32 to Int64, shift_left by 32, add to lo32 *)
      let lo64 = lo32 |> of_int32 in
      let hi64 = hi32 |> of_int32 in
      add lo64 (shift_left hi64 32)
    )

  let skein_ks_parity =
    let hi32 = 0x1BD11BDA |> Int32.of_int in
    let lo32 = 0xA9FC1A22 |> Int32.of_int in
    _skein_mk_64 hi32 lo32

  let rotations = Consts.make
      16L
      42L
      12L
      31L
      16L
      32L
      24L
      21L

  let indices = Consts.make
      1L
      2L
      3L
      4L
      5L
      6L
      7L
      8L

  let rotL x n = Int64.(
      let l = logand n 63L |> to_int in
      let left = shift_left x l in
      let r = (logand (sub 64L n) 63L) |> to_int in
      let right = shift_right x r in
      logor left right
    )

  let add = Int64.add
  let logxor = Int64.logxor

end

module Threefry2xW (T:T) = struct

  (* NOTE needs to be unsigned int32 / int64 *)

  type t = T.t
  type ctr_t = t array
  type key_t = t array
  type ukey_t = t array

  let rand_R nrounds (ctr:ctr_t) (key:key_t) =
    let open T in
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

    let _ = if nrounds>0 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>1 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_1; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>2 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_2; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>3 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_3; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>3 then (
        x0 := add !x0 ks1; x1 := add !x1 ks2;
        x1 := add !x1 indices.i_0;
      ) else () in

    let _ = if nrounds>4 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_4; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>5 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_5; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>6 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_6; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>7 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_7; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>7 then (
        x0 := add !x0 ks2; x1 := add !x1 ks0;
        x1 := add !x1 indices.i_1;
      ) else () in

    let _ = if nrounds>8 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>9 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_1; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>10 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_2; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>11 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_3; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>11 then (
        x0 := add !x0 ks0; x1 := add !x1 ks1;
        x1 := add !x1 indices.i_2;
      ) else () in

    let _ = if nrounds>12 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_4; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>13 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_5; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>14 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_6; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>15 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_7; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>15 then (
        x0 := add !x0 ks1; x1 := add !x1 ks2;
        x1 := add !x1 indices.i_3;
      ) else () in

    let _ = if nrounds>16 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>17 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_1; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>18 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_2; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>19 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_3; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>19 then (
        x0 := add !x0 ks2; x1 := add !x1 ks0;
        x1 := add !x1 indices.i_4;
      ) else () in

    let _ = if nrounds>20 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_4; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>21 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_5; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>22 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_6; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>23 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_7; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>23 then (
        x0 := add !x0 ks0; x1 := add !x1 ks1;
        x1 := add !x1 indices.i_5;
      ) else () in

    let _ = if nrounds>24 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>25 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_1; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>26 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_2; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>27 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_3; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>27 then (
        x0 := add !x0 ks1; x1 := add !x1 ks2;
        x1 := add !x1 indices.i_6;
      ) else () in

    let _ = if nrounds>28 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_4; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>29 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_5; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>30 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_6; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>31 then (x0 := add !x0 !x1; x1 := rotL !x1 rotations.i_7; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>31 then (
        x0 := add !x0 ks2; x1 := add !x1 ks0;
        x1 := add !x1 indices.i_7;
      ) else () in

    [|!x0; !x1|]

  let rand (ctr:ctr_t) (key:key_t) =
    rand_R T.default_rounds ctr key

end

module Threefry2x32 = Threefry2xW(ThreefryInt32)
module Threefry2x64 = Threefry2xW(ThreefryInt64)
