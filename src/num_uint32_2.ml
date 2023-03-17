open Types
module U = Unsigned.UInt32

type (_,_) t = U.t
type digits = digits_two
type word = word_32

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

(* #define SKEIN_KS_PARITY32         0x1BD11BDA *)
let skein_ks_parity = "0x1BD11BDA" |> of_string

let rotations_0 = Rotations.make
    ~to_num:of_int
    13
    15
    26
    6
    17
    29
    16
    24

let rotations_1 = Rotations.zeros ~to_num:of_int

let _31 = 31 |> of_int
let _32 = 32 |> of_int

let rotL x n =
  let l = U.logand n _31 |> to_int in
  let left = U.shift_left x l in
  let r = (U.logand (U.sub _32 n) _31) |> to_int in
  let right = U.shift_right x r in
  U.logor left right
