open Types

type digits = digits_four

module Make (Num:NUM) = struct

  let aux1 lbound nrounds x0 x1 x2 x3 i_0 i_1 =
    (* if(Nrounds>0){                                                      \
     *     X0 += X1; X1 = RotL_##W(X1,R_##W##x4_0_0); X1 ^= X0; \
     *     X2 += X3; X3 = RotL_##W(X3,R_##W##x4_0_1); X3 ^= X2; \
     * }                                                                   \ *)
    if nrounds>lbound then (
      x0 := Num.add !x0 !x1; x1 := Num.rotL !x1 i_0; x1 := Num.logxor !x1 !x0;
      x2 := Num.add !x2 !x3; x3 := Num.rotL !x3 i_1; x3 := Num.logxor !x3 !x2;
    )
    else ()

  let aux2 lbound nrounds x0 x1 x2 x3 i_0 i_1 =
    (* if(Nrounds>1){                                                      \
     *     X0 += X3; X3 = RotL_##W(X3,R_##W##x4_1_0); X3 ^= X0; \
     *     X2 += X1; X1 = RotL_##W(X1,R_##W##x4_1_1); X1 ^= X2; \
     * }                                                                   \ *)
    if nrounds>lbound then (
      x0 := Num.add !x0 !x3; x3 := Num.rotL !x3 i_0; x3 := Num.logxor !x3 !x0;
      x2 := Num.add !x2 !x1; x1 := Num.rotL !x1 i_1; x1 := Num.logxor !x1 !x2;
    )
    else ()

  let aux3 ~of_int lbound nrounds x0 x1 x2 x3 y0 y1 y2 y3 i =
    (* if(Nrounds>3){                                                      \
     *     /* InjectKey(r=1) */                                            \
     *     X0 += ks1; X1 += ks2; X2 += ks3; X3 += ks4; \
     *     X3 += 1;     /* XWCNT4-1 += r  */                 \
     * }                                                                   \ *)
    if nrounds>lbound then (
      x0 := Num.add !x0 y0; x1 := Num.add !x1 y1; x2 := Num.add !x2 y2; x3 := Num.add !x3 y3;
      x3 := Num.add !x3 (of_int i);
    )
    else ()

  let rot_num0 i = Rotations.i Num.rotations_0 ~at:i
  let rot_num1 i = Rotations.i Num.rotations_1 ~at:i

  let rot_num00 = rot_num0 0
  let rot_num01 = rot_num0 1
  let rot_num02 = rot_num0 2
  let rot_num03 = rot_num0 3
  let rot_num04 = rot_num0 4
  let rot_num05 = rot_num0 5
  let rot_num06 = rot_num0 6
  let rot_num07 = rot_num0 7

  let rot_num10 = rot_num1 0
  let rot_num11 = rot_num1 1
  let rot_num12 = rot_num1 2
  let rot_num13 = rot_num1 3
  let rot_num14 = rot_num1 4
  let rot_num15 = rot_num1 5
  let rot_num16 = rot_num1 6
  let rot_num17 = rot_num1 7

  let max_rounds = 72

  let rand_R ~of_int ~rounds:nrounds ~key ~ctr =
    if nrounds > max_rounds then
      let () = Logs.err (fun m -> m "number rounds must be <= %d" max_rounds) in
      [||]
    else (
      let aux3 = aux3 ~of_int in

      let ks4 = Num.skein_ks_parity in
      let ks0 = key.(0) in
      let x0 = ref (Num.add ctr.(0) ks0) in
      let ks4 = Num.logxor ks4 ks0 in

      let ks1 = key.(1) in
      let x1 = ref (Num.add ctr.(1) ks1) in
      let ks4 = Num.logxor ks4 ks1 in

      let ks2 = key.(2) in
      let x2 = ref (Num.add ctr.(2) ks2) in
      let ks4 = Num.logxor ks4 ks2 in

      let ks3 = key.(3) in
      let x3 = ref (Num.add ctr.(3) ks3) in
      let ks4 = Num.logxor ks4 ks3 in

      let _ = aux1  0 nrounds x0 x1 x2 x3 rot_num00 rot_num10 in
      let _ = aux2  1 nrounds x0 x1 x2 x3 rot_num01 rot_num11 in
      let _ = aux1  2 nrounds x0 x1 x2 x3 rot_num02 rot_num12 in
      let _ = aux2  3 nrounds x0 x1 x2 x3 rot_num03 rot_num13 in
      let _ = aux3  3 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 1 in

      let _ = aux1  4 nrounds x0 x1 x2 x3 rot_num04 rot_num14 in
      let _ = aux2  5 nrounds x0 x1 x2 x3 rot_num05 rot_num15 in
      let _ = aux1  6 nrounds x0 x1 x2 x3 rot_num06 rot_num16 in
      let _ = aux2  7 nrounds x0 x1 x2 x3 rot_num07 rot_num17 in
      let _ = aux3  7 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 2 in

      let _ = aux1  8 nrounds x0 x1 x2 x3 rot_num00 rot_num10 in
      let _ = aux2  9 nrounds x0 x1 x2 x3 rot_num01 rot_num11 in
      let _ = aux1 10 nrounds x0 x1 x2 x3 rot_num02 rot_num12 in
      let _ = aux2 11 nrounds x0 x1 x2 x3 rot_num03 rot_num13 in
      let _ = aux3 11 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 3 in

      let _ = aux1 12 nrounds x0 x1 x2 x3 rot_num04 rot_num14 in
      let _ = aux2 13 nrounds x0 x1 x2 x3 rot_num05 rot_num15 in
      let _ = aux1 14 nrounds x0 x1 x2 x3 rot_num06 rot_num16 in
      let _ = aux2 15 nrounds x0 x1 x2 x3 rot_num07 rot_num17 in
      let _ = aux3 15 nrounds x0 x1 x2 x3 ks4 ks0 ks1 ks2 4 in

      let _ = aux1 16 nrounds x0 x1 x2 x3 rot_num00 rot_num10 in
      let _ = aux2 17 nrounds x0 x1 x2 x3 rot_num01 rot_num11 in
      let _ = aux1 18 nrounds x0 x1 x2 x3 rot_num02 rot_num12 in
      let _ = aux2 19 nrounds x0 x1 x2 x3 rot_num03 rot_num13 in
      let _ = aux3 19 nrounds x0 x1 x2 x3 ks0 ks1 ks2 ks3 5 in

      let _ = aux1 20 nrounds x0 x1 x2 x3 rot_num04 rot_num14 in
      let _ = aux2 21 nrounds x0 x1 x2 x3 rot_num05 rot_num15 in
      let _ = aux1 22 nrounds x0 x1 x2 x3 rot_num06 rot_num16 in
      let _ = aux2 23 nrounds x0 x1 x2 x3 rot_num07 rot_num17 in
      let _ = aux3 23 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 6 in

      let _ = aux1 24 nrounds x0 x1 x2 x3 rot_num00 rot_num10 in
      let _ = aux2 25 nrounds x0 x1 x2 x3 rot_num01 rot_num11 in
      let _ = aux1 26 nrounds x0 x1 x2 x3 rot_num02 rot_num12 in
      let _ = aux2 27 nrounds x0 x1 x2 x3 rot_num03 rot_num13 in
      let _ = aux3 27 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 7 in

      let _ = aux1 28 nrounds x0 x1 x2 x3 rot_num04 rot_num14 in
      let _ = aux2 29 nrounds x0 x1 x2 x3 rot_num05 rot_num15 in
      let _ = aux1 30 nrounds x0 x1 x2 x3 rot_num06 rot_num16 in
      let _ = aux2 31 nrounds x0 x1 x2 x3 rot_num07 rot_num17 in
      let _ = aux3 31 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 8 in

      let _ = aux1 32 nrounds x0 x1 x2 x3 rot_num00 rot_num10 in
      let _ = aux2 33 nrounds x0 x1 x2 x3 rot_num01 rot_num11 in
      let _ = aux1 34 nrounds x0 x1 x2 x3 rot_num02 rot_num12 in
      let _ = aux2 35 nrounds x0 x1 x2 x3 rot_num03 rot_num13 in
      let _ = aux3 35 nrounds x0 x1 x2 x3 ks4 ks0 ks1 ks2 9 in

      let _ = aux1 36 nrounds x0 x1 x2 x3 rot_num04 rot_num14 in
      let _ = aux2 37 nrounds x0 x1 x2 x3 rot_num05 rot_num15 in
      let _ = aux1 38 nrounds x0 x1 x2 x3 rot_num06 rot_num16 in
      let _ = aux2 39 nrounds x0 x1 x2 x3 rot_num07 rot_num17 in
      let _ = aux3 39 nrounds x0 x1 x2 x3 ks0 ks1 ks2 ks3 10 in

      let _ = aux1 40 nrounds x0 x1 x2 x3 rot_num00 rot_num10 in
      let _ = aux2 41 nrounds x0 x1 x2 x3 rot_num01 rot_num11 in
      let _ = aux1 42 nrounds x0 x1 x2 x3 rot_num02 rot_num12 in
      let _ = aux2 43 nrounds x0 x1 x2 x3 rot_num03 rot_num13 in
      let _ = aux3 43 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 11 in

      let _ = aux1 44 nrounds x0 x1 x2 x3 rot_num04 rot_num14 in
      let _ = aux2 45 nrounds x0 x1 x2 x3 rot_num05 rot_num15 in
      let _ = aux1 46 nrounds x0 x1 x2 x3 rot_num06 rot_num16 in
      let _ = aux2 47 nrounds x0 x1 x2 x3 rot_num07 rot_num17 in
      let _ = aux3 47 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 12 in

      let _ = aux1 48 nrounds x0 x1 x2 x3 rot_num00 rot_num10 in
      let _ = aux2 49 nrounds x0 x1 x2 x3 rot_num01 rot_num11 in
      let _ = aux1 50 nrounds x0 x1 x2 x3 rot_num02 rot_num12 in
      let _ = aux2 51 nrounds x0 x1 x2 x3 rot_num03 rot_num13 in
      let _ = aux3 51 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 13 in

      let _ = aux1 52 nrounds x0 x1 x2 x3 rot_num04 rot_num14 in
      let _ = aux2 53 nrounds x0 x1 x2 x3 rot_num05 rot_num15 in
      let _ = aux1 54 nrounds x0 x1 x2 x3 rot_num06 rot_num16 in
      let _ = aux2 55 nrounds x0 x1 x2 x3 rot_num07 rot_num17 in
      let _ = aux3 55 nrounds x0 x1 x2 x3 ks4 ks0 ks1 ks2 14 in

      let _ = aux1 56 nrounds x0 x1 x2 x3 rot_num00 rot_num10 in
      let _ = aux2 57 nrounds x0 x1 x2 x3 rot_num01 rot_num11 in
      let _ = aux1 58 nrounds x0 x1 x2 x3 rot_num02 rot_num12 in
      let _ = aux2 59 nrounds x0 x1 x2 x3 rot_num03 rot_num13 in
      let _ = aux3 59 nrounds x0 x1 x2 x3 ks0 ks1 ks2 ks3 15 in

      let _ = aux1 60 nrounds x0 x1 x2 x3 rot_num04 rot_num14 in
      let _ = aux2 61 nrounds x0 x1 x2 x3 rot_num05 rot_num15 in
      let _ = aux1 62 nrounds x0 x1 x2 x3 rot_num06 rot_num16 in
      let _ = aux2 63 nrounds x0 x1 x2 x3 rot_num07 rot_num17 in
      let _ = aux3 63 nrounds x0 x1 x2 x3 ks1 ks2 ks3 ks4 16 in

      let _ = aux1 64 nrounds x0 x1 x2 x3 rot_num00 rot_num10 in
      let _ = aux2 65 nrounds x0 x1 x2 x3 rot_num01 rot_num11 in
      let _ = aux1 66 nrounds x0 x1 x2 x3 rot_num02 rot_num12 in
      let _ = aux2 67 nrounds x0 x1 x2 x3 rot_num03 rot_num13 in
      let _ = aux3 67 nrounds x0 x1 x2 x3 ks2 ks3 ks4 ks0 17 in

      let _ = aux1 68 nrounds x0 x1 x2 x3 rot_num04 rot_num14 in
      let _ = aux2 69 nrounds x0 x1 x2 x3 rot_num05 rot_num15 in
      let _ = aux1 70 nrounds x0 x1 x2 x3 rot_num06 rot_num16 in
      let _ = aux2 71 nrounds x0 x1 x2 x3 rot_num07 rot_num17 in
      let _ = aux3 71 nrounds x0 x1 x2 x3 ks3 ks4 ks0 ks1 18 in

      [|!x0; !x1; !x2; !x3|]
    )
end
