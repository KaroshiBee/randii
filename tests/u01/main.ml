module Rng = Randii.Rng
module T = Rng.Threefry_4x32

let key = ref @@ T.of_int_array [|0;0;0;0|]
let ctr = ref @@ T.of_int_array [|0;0;0;0|]
let xs = ref [||]
let i = ref 3

let pp xs =
  String.concat "," (Array.to_list xs |> List.map string_of_int)

let rec aux () =
  let () = Logs.debug (fun m -> m "aux %d ()- got xs: %s\n" !i @@ pp !xs) in
  if (Array.length !xs < 1 || !i < 0) then
    let () = Logs.debug (fun m -> m "aux %d ()- resetting xs/ctr\n" !i) in
    let () = xs := T.uniform ~key:!key ~ctr:!ctr () in
    let () = ctr := T.succ !ctr in
    let () = i := 3 in
    aux ()
  else
    try
      let () = Logs.debug (fun m -> m "aux %d ()- trying to get value\n" !i) in
      let v = Array.get !xs !i in
      let () = Logs.debug (fun m -> m "aux %d ()- got value %d\n" !i v) in
      let () = i := !i -1 in
      v
    with Invalid_argument s ->
      let () = Logs.debug (fun m -> m "aux %d ()- got error %s\n" !i s) in
      let () = i := !i -1 in
      aux ()

let bits () =
  aux ()

let () =
  let open TestU01 in
  let open Probdist in

  let gen = Unif01.create_extern_gen_bits "randii" bits in
  Gofw.set_suspectp 0.01;
  Bbattery.small_crush gen
