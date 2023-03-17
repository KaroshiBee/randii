
module Make (T:Cbrn.Types.GEN) = struct
  let key = ref @@ T.of_int_array @@ Array.init T.digits (fun _ -> 0)
  let ctr = ref @@ T.of_int_array @@ Array.init T.digits (fun _ -> 0)
  let xs = ref [||]
  let i = ref (T.digits - 1)

  let reset () =
    let () = key := T.of_int_array @@ Array.init T.digits (fun _ -> 0) in
    let () = ctr := T.of_int_array @@ Array.init T.digits (fun _ -> 0) in
    let () = xs := [||] in
    let () = i := T.digits-1 in
    ()

  let pp xs =
    String.concat "," (Array.to_list xs |> List.map string_of_int)

  let rec bits () =
    let () = Logs.debug (fun m -> m "bits %d ()- got xs: %s\n" !i @@ pp !xs) in
    if (Array.length !xs < 1 || !i < 0) then
      let () = Logs.debug (fun m -> m "bits %d ()- resetting xs/ctr\n" !i) in
      let () = xs := T.uniform ~key:!key ~ctr:!ctr () in
      let () = ctr := T.succ !ctr in
      let () =
        if T.is_zero !ctr then
          let () = Logs.debug (fun m -> m "bits %d ()- incr key\n" !i) in
          key := T.succ !key
        else
          ()
      in
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

module Rng_2x32 = Make (Cbrn.Rng.Threefry_2x32)
module Rng_2x64 = Make (Cbrn.Rng.Threefry_2x64)
module Rng_4x32 = Make (Cbrn.Rng.Threefry_4x32)
module Rng_4x64 = Make (Cbrn.Rng.Threefry_4x64)

open TestU01

let () =
  let () = Rng_2x32.reset () in
  let gen = Unif01.create_extern_gen_bits "randii 2x32" Rng_2x32.bits in
  Bbattery.small_crush gen

let () =
  let () = Rng_2x64.reset () in
  let gen = Unif01.create_extern_gen_bits "randii 2x64" Rng_2x64.bits in
  Bbattery.small_crush gen

let () =
  let () = Rng_4x32.reset () in
  let gen = Unif01.create_extern_gen_bits "randii 4x32" Rng_4x32.bits in
  Bbattery.small_crush gen

let () =
  let () = Rng_4x64.reset () in
  let gen = Unif01.create_extern_gen_bits "randii 4x64" Rng_4x64.bits in
  Bbattery.small_crush gen
