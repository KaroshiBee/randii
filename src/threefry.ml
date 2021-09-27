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
    (* rotation amounts or increments *)
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

end

(* factor out dependence on UInt32 / UInt64 *)
module type T = sig
  type t (* UInt32 or UInt64 *)
  val default_rounds : int
  val skein_ks_parity : t
  val rotations : t Consts.t
  val indices : t Consts.t

  val rotL : t -> t -> t
  val add : t -> t -> t
  val logxor : t -> t -> t

end

module UInt32_T = struct

  type t = Unsigned.UInt32.t

  let default_rounds = 20
  let _31 = 31 |> Unsigned.UInt32.of_int
  let _32 = 32 |> Unsigned.UInt32.of_int

  (* #define SKEIN_KS_PARITY32         0x1BD11BDA *)
  let skein_ks_parity = 0x1BD11BDA |> Unsigned.UInt32.of_int

  let rotations = Consts.make
      Unsigned.UInt32.of_int
      13
      15
      26
      6
      17
      29
      16
      24

  let indices = Consts.make
      Unsigned.UInt32.of_int
      1
      2
      3
      4
      5
      6
      7
      8

  let rotL x n = Unsigned.UInt32.(
      let l = logand n _31 |> to_int in
      let left = shift_left x l in
      let r = (logand (sub _32 n) _31) |> to_int in
      let right = shift_right x r in
      logor left right
    )

  let add = Unsigned.UInt32.add
  let logxor = Unsigned.UInt32.logxor

end

module UInt64_T = struct

  type t = Unsigned.UInt64.t

  let default_rounds = 20
  let _63 = 63 |> Unsigned.UInt64.of_int
  let _64 = 64 |> Unsigned.UInt64.of_int

  (* let _skein_mk_64 hi32 lo32 = Unsigned.UInt64.(
   *     (\* #define SKEIN_MK_64(hi32,lo32)  ((lo32) + (((uint64_t) (hi32)) << 32))
   *      * #define SKEIN_KS_PARITY64         SKEIN_MK_64(0x1BD11BDA,0xA9FC1A22) *\)
   *     (\* convert hi32 to Int64, shift_left by 32, add to lo32 *\)
   *     let lo64 = lo32 |> of_uint32 in
   *     let hi64 = hi32 |> of_uint32 in
   *     add lo64 (shift_left hi64 32)
   *   )
   *
   * let skein_ks_parity =
   *   let hi32 = 0x1BD11BDA |> Unsigned.UInt32.of_int in
   *   let lo32 = 0xA9FC1A22 |> Unsigned.UInt32.of_int in
   *   _skein_mk_64 hi32 lo32 *)

  let skein_ks_parity = "2004413935125273122" |> Unsigned.UInt64.of_string

  let rotations = Consts.make
      Unsigned.UInt64.of_int
      16
      42
      12
      31
      16
      32
      24
      21

  let indices = Consts.make
      Unsigned.UInt64.of_int
      1
      2
      3
      4
      5
      6
      7
      8

  let rotL x n = Unsigned.UInt64.(
      let l = logand n _63 |> to_int in
      let left = shift_left x l in
      let r = (logand (sub _64 n) _63) |> to_int in
      let right = shift_right x r in
      logor left right
    )

  let add = Unsigned.UInt64.add
  let logxor = Unsigned.UInt64.logxor

end

(*
 * NOTE needs to be unsigned int32 / int64
 * Probably need a module for ctr_t and key_t
 * to stop incorrect constructions and allow for incr operation *)

module type RAND_T = sig
  type ctr_t
  type key_t
  val rand : ctr_t -> key_t -> ctr_t
end

module type RAND_TEST_T = sig
  include RAND_T
  val rand_R : int -> ctr_t -> key_t -> ctr_t
end

module Make_threefry2xW_TEST (T:T) : (RAND_TEST_T with type ctr_t := T.t array and type key_t := T.t array) = struct

  type t = T.t
  type ctr_t = t array
  type key_t = t array

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

module Make_threefry2xW (T:T) : (RAND_T with type ctr_t := T.t array and type key_t := T.t array) = struct
  include Make_threefry2xW_TEST(T)
end

module Threefry2x32 = Make_threefry2xW(UInt32_T)
module Threefry2x64 = Make_threefry2xW(UInt64_T)

module Threefry2x32_TEST = Make_threefry2xW_TEST(UInt32_T)
module Threefry2x64_TEST = Make_threefry2xW_TEST(UInt64_T)
