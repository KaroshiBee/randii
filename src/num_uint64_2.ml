open Types
module U = Unsigned.UInt64

type (_,_) t = U.t
type digits = digits_two
type word = word_64

let digits = 2

let of_int = U.of_int
let of_string = U.of_string

let to_int = U.to_int
let to_string = U.to_string

let zero = of_int 0
let one = of_int 1
let max_int = U.max_int
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

let rotations_0 = Rotations.make
    ~to_num:of_int
    16
    42
    12
    31
    16
    32
    24
    21

let rotations_1 = Rotations.zeros ~to_num:of_int

let _63 = 63 |> of_int
let _64 = 64 |> of_int

let rotL x n =
  let l = U.logand n _63 |> to_int in
  let left = U.shift_left x l in
  let r = (U.logand (U.sub _64 n) _63) |> to_int in
  let right = U.shift_right x r in
  U.logor left right
