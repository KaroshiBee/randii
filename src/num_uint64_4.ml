open Types
include Num_uint64_2

type digits = digits_four
type word = word_64

let digits = 4
let rotations_0 = Rotations.make
    ~to_num:of_int
    14
    52
    23
    5
    25
    46
    58
    32

let rotations_1 = Rotations.make
    ~to_num:of_int
    16
    57
    40
    37
    33
    12
    22
    32
