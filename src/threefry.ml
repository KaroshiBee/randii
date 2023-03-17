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


open Types

let default_rounds = 20
let default_upper = Unsigned.UInt32.(shift_left one 30 |> to_int)
let default_upper_float = float_of_int default_upper

module Make
    (Num:NUM)
    (R:RNG_MAKER with type digits = Num.digits)
  : GEN = struct

  module Rng = R.Make(Num)

  type t = (Num.digits, Num.word) Num.t array

  let of_int_array arr =
    if (Num.digits <> Array.length arr) then
      raise @@ Invalid_argument (Printf.sprintf "Need %d digit ctr/key" Num.digits)
    else
      Array.map Num.of_int arr
  let to_int_array = Array.map Num.to_int

  let of_string_array arr =
    if (Num.digits <> Array.length arr) then
      raise @@ Invalid_argument (Printf.sprintf "Need %d digit ctr/key" Num.digits)
    else
      Array.map Num.of_string arr
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
  let pred t = ctr ~equal:Num.equal 0 Num.pred Num.max_int t; t

  let rand ?(rounds=default_rounds) ~key ~ctr () =
    Rng.rand_R ~of_int:Num.of_int ~rounds ~key ~ctr

  let limit n = Num.(sub max_int (rem max_int n))

  let unbiased ~key ~ctr upper r =
    let u = Num.of_int upper in
    try
      (* find first rand less than limit
       * and do the remainder with that number
       * error if no number is found *)
      Array.to_list r
      |> List.filter (fun x -> x <= limit u)
      |> List.map (fun x -> Num.rem x u)
      |> Array.of_list
    with
    | Invalid_argument _ ->
      let to_str ctr = Printf.sprintf "{%s}" @@ String.concat "," (Array.to_list @@ to_string_array ctr) in
      let () = Logs.warn (fun m -> m "Bad key/ctr pair ( %s / %s) for given ~upper:%d" (to_str key) (to_str ctr) upper) in
      [||]

  let uniform ?(upper=default_upper) ?(rounds=default_rounds) ~key ~ctr () =
    unbiased ~key ~ctr upper @@ rand ~rounds ~key ~ctr () |> to_int_array

  let uniform01 ?(rounds=default_rounds) ~key ~ctr () =
    let arr = unbiased ~key ~ctr default_upper @@ rand ~rounds ~key ~ctr () in
    Array.map (fun x -> let f = Num.to_int x |> float_of_int in f /. default_upper_float) arr

end

module Gen_2_32 = Make (Num_uint32_2) (Rng_threefry_2_digits)
module Gen_4_32 = Make (Num_uint32_4) (Rng_threefry_4_digits)
module Gen_2_64 = Make (Num_uint64_2) (Rng_threefry_2_digits)
module Gen_4_64 = Make (Num_uint64_4) (Rng_threefry_4_digits)
