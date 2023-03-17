open Types

type digits = digits_two

module Make (Num:NUM) = struct

  let aux1 lbound nrounds x0 x1 i =
    (* if(Nrounds>0){  X0 += X1; X1 = RotL_##W(X1,R_##W##x2_0_0); X1 ^= X0; }  *)
    if nrounds>lbound then (
      x0 := Num.add !x0 !x1; x1 := Num.rotL !x1 i; x1 := Num.logxor !x1 !x0;
    )
    else ()

  let aux3 ~of_int lbound nrounds x0 x1 ks0 ks1 i =
    (*
     if(Nrounds>3){                                                      \
        /* InjectKey(r=1) */                                            \
        X0 += ks1; X1 += ks2;                               \
        X1 += 1;     /* X.v[2-1] += r  */                   \
     }                                                                   \
    *)
    if nrounds>lbound then (
      x0 := Num.add !x0 ks0; x1 := Num.add !x1 ks1;
      x1 := Num.add !x1 (of_int i)
    )
    else ()

  let rot_num0 i = Rotations.i Num.rotations_0 ~at:i

  let rot_num00 = rot_num0 0
  let rot_num01 = rot_num0 1
  let rot_num02 = rot_num0 2
  let rot_num03 = rot_num0 3
  let rot_num04 = rot_num0 4
  let rot_num05 = rot_num0 5
  let rot_num06 = rot_num0 6
  let rot_num07 = rot_num0 7

  let max_rounds = 32

  let rand_R ~of_int ~rounds:nrounds ~key ~ctr =
    if nrounds > max_rounds then
      let () = Logs.err (fun m -> m "number rounds must be <= %d" max_rounds) in
      [||]
    else (
      let aux3 = aux3 ~of_int in

      let ks2 = Num.skein_ks_parity in
      let ks0 = key.(0) in
      let x0 = ref (Num.add ctr.(0) ks0) in
      let ks2 = Num.logxor ks2 ks0 in

      let ks1 = key.(1) in
      let x1 = ref (Num.add ctr.(1) ks1) in
      let ks2 = Num.logxor ks2 ks1 in

      let _ = aux1  0 nrounds x0 x1 rot_num00 in
      let _ = aux1  1 nrounds x0 x1 rot_num01 in
      let _ = aux1  2 nrounds x0 x1 rot_num02 in
      let _ = aux1  3 nrounds x0 x1 rot_num03 in
      let _ = aux3  3 nrounds x0 x1 ks1 ks2 1 in

      let _ = aux1  4 nrounds x0 x1 rot_num04 in
      let _ = aux1  5 nrounds x0 x1 rot_num05 in
      let _ = aux1  6 nrounds x0 x1 rot_num06 in
      let _ = aux1  7 nrounds x0 x1 rot_num07 in
      let _ = aux3  7 nrounds x0 x1 ks2 ks0 2 in

      let _ = aux1  8 nrounds x0 x1 rot_num00 in
      let _ = aux1  9 nrounds x0 x1 rot_num01 in
      let _ = aux1 10 nrounds x0 x1 rot_num02 in
      let _ = aux1 11 nrounds x0 x1 rot_num03 in
      let _ = aux3 11 nrounds x0 x1 ks0 ks1 3 in

      let _ = aux1 12 nrounds x0 x1 rot_num04 in
      let _ = aux1 13 nrounds x0 x1 rot_num05 in
      let _ = aux1 14 nrounds x0 x1 rot_num06 in
      let _ = aux1 15 nrounds x0 x1 rot_num07 in
      let _ = aux3 15 nrounds x0 x1 ks1 ks2 4 in

      let _ = aux1 16 nrounds x0 x1 rot_num00 in
      let _ = aux1 17 nrounds x0 x1 rot_num01 in
      let _ = aux1 18 nrounds x0 x1 rot_num02 in
      let _ = aux1 19 nrounds x0 x1 rot_num03 in
      let _ = aux3 19 nrounds x0 x1 ks2 ks0 5 in

      let _ = aux1 20 nrounds x0 x1 rot_num04 in
      let _ = aux1 21 nrounds x0 x1 rot_num05 in
      let _ = aux1 22 nrounds x0 x1 rot_num06 in
      let _ = aux1 23 nrounds x0 x1 rot_num07 in
      let _ = aux3 23 nrounds x0 x1 ks0 ks1 6 in

      let _ = aux1 24 nrounds x0 x1 rot_num00 in
      let _ = aux1 25 nrounds x0 x1 rot_num01 in
      let _ = aux1 26 nrounds x0 x1 rot_num02 in
      let _ = aux1 27 nrounds x0 x1 rot_num03 in
      let _ = aux3 27 nrounds x0 x1 ks1 ks2 7 in

      let _ = aux1 28 nrounds x0 x1 rot_num04 in
      let _ = aux1 29 nrounds x0 x1 rot_num05 in
      let _ = aux1 30 nrounds x0 x1 rot_num06 in
      let _ = aux1 31 nrounds x0 x1 rot_num07 in
      let _ = aux3 31 nrounds x0 x1 ks2 ks0 8 in

      [|!x0; !x1|]
    )
end
