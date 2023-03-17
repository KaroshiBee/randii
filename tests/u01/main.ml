
module Make (T:Randii.Types.GEN) = struct
  let key = ref @@ T.of_int_array @@ Array.init T.digits (fun _ -> 0)
  let ctr = ref @@ T.of_int_array @@ Array.init T.digits (fun _ -> 0)
  let xs = ref [||]
  let i = ref (T.digits - 1)

  let pp xs =
    String.concat "," (Array.to_list xs |> List.map string_of_int)

  let rec bits () =
    let () = Logs.debug (fun m -> m "bits %d ()- got xs: %s\n" !i @@ pp !xs) in
    if (Array.length !xs < 1 || !i < 0) then
      let () = Logs.debug (fun m -> m "bits %d ()- resetting xs/ctr\n" !i) in
      let () = xs := T.uniform ~key:!key ~ctr:!ctr () in
      let () = ctr := T.succ !ctr in
      let () = i := T.digits-1 in
      bits ()
    else
      try
        let () = Logs.debug (fun m -> m "bits %d ()- trying to get value\n" !i) in
        let v = Array.get !xs !i in
        let () = Logs.debug (fun m -> m "bits %d ()- got value %d\n" !i v) in
        let () = i := !i -1 in
        v
      with Invalid_argument s ->
        let () = Logs.debug (fun m -> m "bits %d ()- got error %s\n" !i s) in
        let () = i := !i -1 in
        bits ()
end

module Rng_2x32 = Make (Randii.Rng.Threefry_2x32)

open TestU01
open Probdist

let () =
  let gen = Unif01.create_extern_gen_bits "randii 2x32" Rng_2x32.bits in
  Gofw.set_suspectp 0.01;
  Bbattery.small_crush gen
