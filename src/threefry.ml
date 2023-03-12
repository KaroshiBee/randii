(*
 * This ml file is an implementation of the Threefry algorithm as described in
 *
 *  "Parallel Random Numbers: As Easy as 1, 2, 3,
 *  Salmon, Moraes, Dror & Shaw, SC11, Seattle, Washington, USA, 2011, ACM "
 *
 * using the implementation at (as of 2021)
 * https://github.com/DEShawResearch/random123
 *
 * The original Threefry header file this is taken from is
 * random123/include/Random123/threefry.h
 * of that same project and carries the following notice:
 *
 * /*
 * Copyright 2010-2011, D. E. Shaw Research.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions, and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions, and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * * Neither the name of D. E. Shaw Research nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * */
 *
 * /** \cond HIDDEN_FROM_DOXYGEN */
 * /* Significant parts of this file were copied from
 *    from:
 *       Skein_FinalRnd/ReferenceImplementation/skein.h
 *       Skein_FinalRnd/ReferenceImplementation/skein_block.c
 *
 *    in http://csrc.nist.gov/groups/ST/hash/sha-3/Round3/documents/Skein_FinalRnd.zip
 *
 *    This file has been modified so that it may no longer perform its originally
 *    intended function.  If you're looking for a Skein or Threefish source code,
 *    please consult the original file.
 *
 *    The original file had the following header:
 * **************************************************************************
 * **
 * ** Interface declarations and internal definitions for Skein hashing.
 * **
 * ** Source code author: Doug Whiting, 2008.
 * **
 * ** This algorithm and source code is released to the public domain.
 * **
 * ***************************************************************************
 *
 * */
 *  *)

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

  let make f i_0 i_1 i_2 i_3 i_4 i_5 i_6 i_7 = {
    i_0=f i_0;
    i_1=f i_1;
    i_2=f i_2;
    i_3=f i_3;
    i_4=f i_4;
    i_5=f i_5;
    i_6=f i_6;
    i_7=f i_7;
  }

  let zeros f = {
    i_0=f 0;
    i_1=f 0;
    i_2=f 0;
    i_3=f 0;
    i_4=f 0;
    i_5=f 0;
    i_6=f 0;
    i_7=f 0;
  }

end

type two_digits
type four_digits


(* factor out dependence on UInt32 / UInt64 *)
module type T = sig
  type digits
  type t (* UInt32 or UInt64 *)
  val of_int : int -> t
  val to_int : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val succ : t -> t
  val pred : t -> t
  val zero : t
  val one : t
  val max_int : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val rem : t -> t -> t
  val logxor : t -> t -> t

  val default_rounds : int
  val skein_ks_parity : t
  val rotations_0 : t Consts.t
  val rotations_1 : t Consts.t

  val rotL : t -> t -> t

end

module type T2 = sig
  include T
  type digits = two_digits
end

module type T4 = sig
  include T
  type digits = four_digits
end


module UInt32_2_T = struct
  include Unsigned.UInt32

  type digits = two_digits

  let default_rounds = 20
  let _31 = 31 |> of_int
  let _32 = 32 |> of_int

  (* #define SKEIN_KS_PARITY32         0x1BD11BDA *)
  let skein_ks_parity = 0x1BD11BDA |> of_int

  let rotations_0 = Consts.make
      of_int
      13
      15
      26
      6
      17
      29
      16
      24

  let rotations_1 = Consts.zeros of_int

  let rotL x n =
    let l = logand n _31 |> to_int in
    let left = shift_left x l in
    let r = (logand (sub _32 n) _31) |> to_int in
    let right = shift_right x r in
    logor left right

end

module UInt32_4_T = struct
  include UInt32_2_T

  type digits = four_digits

  let rotations_0 = Consts.make
      of_int
      10
      11
      13
      23
      6
      17
      25
      18

  let rotations_1 = Consts.make
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

module UInt64_2_T = struct
  include Unsigned.UInt64

  type digits = two_digits

  let default_rounds = 20
  let _63 = 63 |> of_int
  let _64 = 64 |> of_int

  (* #define SKEIN_MK_64(hi32,lo32)  ((lo32) + (((uint64_t) (hi32)) << 32))
   * #define SKEIN_KS_PARITY64         SKEIN_MK_64(0x1BD11BDA,0xA9FC1A22) *)
  (* convert hi32 to Int64, shift_left by 32, add to lo32 *)
  let skein_ks_parity =
    let hi = 0x1BD11BDA |> of_int in
    let lo = 0xA9FC1A22 |> of_int in
    add lo (shift_left hi 32)

  let rotations_0 = Consts.make
      of_int
      16
      42
      12
      31
      16
      32
      24
      21

  let rotations_1 = Consts.zeros of_int

  let rotL x n =
    let l = logand n _63 |> to_int in
    let left = shift_left x l in
    let r = (logand (sub _64 n) _63) |> to_int in
    let right = shift_right x r in
    logor left right

end

module UInt64_4_T = struct
  include UInt64_2_T

  type digits = four_digits

  let rotations_0 = Consts.make
      of_int
      14
      52
      23
      5
      25
      46
      58
      32

  let rotations_1 = Consts.make
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

(*
 * NOTE needs to be unsigned int32 / int64
 * Probably need a module for ctr_t and key_t
 * to stop incorrect constructions and allow for incr operation
 *
 * NOTE also that this implementation swaps around the args key/ctr *)

module type RAND_T = sig
  type ctr_t
  type key_t
  val rand : key_t -> ctr_t -> ctr_t
end

(* expose rand_R for testing purposes *)
module type RAND_TEST_T = sig
  include RAND_T
  val rand_R : int -> key_t -> ctr_t -> ctr_t
end

module Make_threefry2xW_TEST (T:T2) : (RAND_TEST_T with type ctr_t := T.t array and type key_t := T.t array) = struct

  type t = T.t
  type ctr_t = t array
  type key_t = t array

  let _aux1 lbound nrounds x0 x1 i =
    (* if(Nrounds>0){  X0 += X1; X1 = RotL_##W(X1,R_##W##x2_0_0); X1 ^= X0; }  *)
    if nrounds>lbound then T.(
        x0 := add !x0 !x1; x1 := rotL !x1 i; x1 := logxor !x1 !x0;
      )
    else ()

  let _aux3 lbound nrounds x0 x1 ks0 ks1 i =
    (*
     if(Nrounds>3){                                                      \
        /* InjectKey(r=1) */                                            \
        X0 += ks1; X1 += ks2;                               \
        X1 += 1;     /* X.v[2-1] += r  */                   \
     }                                                                   \
    *)
    if nrounds>lbound then T.(
        x0 := add !x0 ks0; x1 := add !x1 ks1;
        x1 := add !x1 (of_int i)
      )
    else ()

  let rand_R nrounds (key:key_t) (ctr:ctr_t) =
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

  let rand (key:key_t) (ctr:ctr_t) =
    rand_R T.default_rounds key ctr

end

module Make_threefry4xW_TEST (T:T4) : (RAND_TEST_T with type ctr_t := T.t array and type key_t := T.t array) = struct

  type t = T.t
  type ctr_t = t array
  type key_t = t array

  let _aux1 lbound nrounds x0 x1 x2 x3 i_0 i_1 =
    (* if(Nrounds>0){                                                      \
     *     X0 += X1; X1 = RotL_##W(X1,R_##W##x4_0_0); X1 ^= X0; \
     *     X2 += X3; X3 = RotL_##W(X3,R_##W##x4_0_1); X3 ^= X2; \
     * }                                                                   \ *)
    if nrounds>lbound then T.(
        x0 := add !x0 !x1; x1 := rotL !x1 i_0; x1 := logxor !x1 !x0;
        x2 := add !x2 !x3; x3 := rotL !x3 i_1; x3 := logxor !x3 !x2;
      )
    else ()

  let _aux2 lbound nrounds x0 x1 x2 x3 i_0 i_1 =
    (* if(Nrounds>1){                                                      \
     *     X0 += X3; X3 = RotL_##W(X3,R_##W##x4_1_0); X3 ^= X0; \
     *     X2 += X1; X1 = RotL_##W(X1,R_##W##x4_1_1); X1 ^= X2; \
     * }                                                                   \ *)
    if nrounds>lbound then T.(
        x0 := add !x0 !x3; x3 := rotL !x3 i_0; x3 := logxor !x3 !x0;
        x2 := add !x2 !x1; x1 := rotL !x1 i_1; x1 := logxor !x1 !x2;
      )
    else ()

  let _aux3 lbound nrounds x0 x1 x2 x3 y0 y1 y2 y3 i =
    (* if(Nrounds>3){                                                      \
     *     /* InjectKey(r=1) */                                            \
     *     X0 += ks1; X1 += ks2; X2 += ks3; X3 += ks4; \
     *     X3 += 1;     /* XWCNT4-1 += r  */                 \
     * }                                                                   \ *)
    if nrounds>lbound then T.(
        x0 := add !x0 y0; x1 := add !x1 y1; x2 := add !x2 y2; x3 := add !x3 y3;
        x3 := add !x3 (of_int i);
      )
    else ()

  let rand_R nrounds (key:key_t) (ctr:ctr_t) =
    let open T in
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


  let rand (key:key_t) (ctr:ctr_t) =
    rand_R T.default_rounds key ctr

end


module Make_threefry2xW (T:T2) : (RAND_T with type ctr_t := T.t array and type key_t := T.t array) = struct
  include Make_threefry2xW_TEST(T)
end

module Make_threefry4xW (T:T4) : (RAND_T with type ctr_t := T.t array and type key_t := T.t array) = struct
  include Make_threefry4xW_TEST(T)
end

module Threefry2x32 = Make_threefry2xW(UInt32_2_T)
module Threefry2x64 = Make_threefry2xW(UInt64_2_T)

module Threefry4x32 = Make_threefry4xW(UInt32_4_T)
module Threefry4x64 = Make_threefry4xW(UInt64_4_T)
