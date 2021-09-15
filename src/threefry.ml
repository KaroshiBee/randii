module Threefry2x32 = struct

  (* NOTE needs to be unsigned int32 *)
  let default_rounds = 20
  let skein_ks_parity32 = 0x1BD11BDA |> Int32.of_int

  let r_32x2_0_0 = 13l
  let r_32x2_1_0 = 15l
  let r_32x2_2_0 = 26l
  let r_32x2_3_0 =  6l
  let r_32x2_4_0 = 17l
  let r_32x2_5_0 = 29l
  let r_32x2_6_0 = 16l
  let r_32x2_7_0 = 24l


  type t = Int32.t
  type ctr_t = t array
  type key_t = t array
  type ukey_t = t array

  let rotL x n = Int32.(
      let l = logand n 31l |> to_int in
      let left = shift_left x l in
      let r = (logand (sub 32l n) 31l) |> to_int in
      let right = shift_right x r in
      logor left right
    )

  let rand_R nrounds (ctr:ctr_t) (key:key_t) =
    let open Int32 in
    let _ = if nrounds > 32 then
        failwith @@ Printf.sprintf
          "number rounds must be <= 32"
      else ()
    in
    let ks2 = skein_ks_parity32 in
    let ks0 = key.(0) in
    let x0 = ref (add ctr.(0) ks0) in
    let ks2 = logxor ks2 ks0 in

    let ks1 = key.(1) in
    let x1 = ref (add ctr.(1) ks1) in
    let ks2 = logxor ks2 ks1 in

    let _ = if nrounds>0 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_0_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>1 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_1_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>2 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_2_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>3 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_3_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>3 then (
        x0 := add !x0 ks1; x1 := add !x1 ks2;
        x1 := add !x1 1l;
      ) else () in

    let _ = if nrounds>4 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_4_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>5 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_5_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>6 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_6_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>7 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_7_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>7 then (
        x0 := add !x0 ks2; x1 := add !x1 ks0;
        x1 := add !x1 2l;
      ) else () in

    let _ = if nrounds>8 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_0_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>9 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_1_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>10 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_2_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>11 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_3_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>11 then (
        x0 := add !x0 ks0; x1 := add !x1 ks1;
        x1 := add !x1 3l;
      ) else () in

    let _ = if nrounds>12 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_4_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>13 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_5_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>14 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_6_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>15 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_7_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>15 then (
        x0 := add !x0 ks1; x1 := add !x1 ks2;
        x1 := add !x1 4l;
      ) else () in

    let _ = if nrounds>16 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_0_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>17 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_1_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>18 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_2_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>19 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_3_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>19 then (
        x0 := add !x0 ks2; x1 := add !x1 ks0;
        x1 := add !x1 5l;
      ) else () in

    let _ = if nrounds>20 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_4_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>21 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_5_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>22 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_6_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>23 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_7_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>23 then (
        x0 := add !x0 ks0; x1 := add !x1 ks1;
        x1 := add !x1 6l;
      ) else () in

    let _ = if nrounds>24 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_0_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>25 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_1_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>26 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_2_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>27 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_3_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>27 then (
        x0 := add !x0 ks1; x1 := add !x1 ks2;
        x1 := add !x1 7l;
      ) else () in

    let _ = if nrounds>28 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_4_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>29 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_5_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>30 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_6_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>31 then (x0 := add !x0 !x1; x1 := rotL !x1 r_32x2_7_0; x1 := logxor !x1 !x0;) else () in
    let _ = if nrounds>31 then (
        x0 := add !x0 ks2; x1 := add !x1 ks0;
        x1 := add !x1 8l;
      ) else () in

    [|!x0; !x1|]

  let rand (ctr:ctr_t) (key:key_t) =
    rand_R default_rounds ctr key

end
