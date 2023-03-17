open TestU01
open Probdist

let () =
  let gen = Unif01.create_extern_gen_bits "stdlib" Random.bits in
  Gofw.set_suspectp 0.01;
  Bbattery.small_crush gen
