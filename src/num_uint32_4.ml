open Types
include Num_uint32_2

type digits = digits_four
type word = word_32

let digits = 4
let rotations_0 = Rotations.make
    ~to_num:of_int
    10
    11
    13
    23
    6
    17
    25
    18

let rotations_1 = Rotations.make
    ~to_num:of_int
    26
    21
    27
    5
    20
    11
    10
    20
