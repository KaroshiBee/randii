module R = Cbrn.Threefry.Gen_4_64

let key = [| "0";"0";"0";"0" |] |> R.of_string_array
let ctr = [| "1";"0";"0";"0" |] |> R.of_string_array

let r = R.rand
let u = R.uniform ~upper:10
