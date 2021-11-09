open Unsigned.UInt64

module R = Randii.Rng.Uniform
module Ctr = R.Ctr

let zs = [|zero; zero; zero; zero; |] |> Ctr.of_array
let ky = Ctr.copy zs
let ctr = Ctr.copy zs

let u = R.discrete ~upper:(of_int 10)
