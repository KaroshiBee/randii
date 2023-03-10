module R = Randii.Rng.Threefry_4x64

let key = [| 0; 0; 0; 0; |] |> R.counter |> Result.get_ok
let ctr = [| 1; 0; 0; 0; |] |> R.counter |> Result.get_ok

let r = R.rand
let u = R.uniform ~upper:10
