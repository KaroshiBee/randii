open Unsigned.UInt64

module R = Randii.Rng.U4x64
module Ctr = R.Ctr

let zs = [|zero; zero; zero; zero; |] |> Ctr.of_array |> Result.get_ok
let ky = Ctr.copy zs
let ctr = Ctr.copy zs

let u = R.discrete ~upper:(of_int 10)
