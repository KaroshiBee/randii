module Ctr_test (I:Randii.Threefry.T) = struct

  module C = Randii.Ctr.Make_ctr(I)

  let test_of_array () =
    let z = I.zero in
    let o = I.one in
    let eg1 = [|o|] |> C.of_array in
    let eg2 = [|o;z|] |> Array.map I.to_string in
    let eg3 = [|o;z;z|] |> C.of_array in
    let eg4 = [|o;z;z;z|] |> Array.map I.to_string in
    Alcotest.(check (array string))
      "various constructors 1/2"
      eg2 (eg1 |> C.to_string_array);

    Alcotest.(check (array string))
      "various constructors 3/4"
      eg4 (eg3 |> C.to_string_array)

  let test_succ () =
    let z = I.zero in
    let zs = [|z;z;z;z|] |> C.of_array in
    let expected = [|I.one;z;z;z|] |> Array.map I.to_string in
    let zs = C.succ zs in
    let actual = C.to_string_array zs in
    Alcotest.(check (array string))
      "succ 0 with 4 digits"
      expected actual

  let test_succ_rollover () =
    let z = I.zero in
    let m = I.max_int in
    let ms = [|m;m;m;m|] |> C.of_array in
    let expected = [|z;z;z;z|] |> Array.map I.to_string in
    let ms = C.succ ms in
    let actual = C.to_string_array ms in
    Alcotest.(check (array string))
      "succ max with 4 digits"
      expected actual

  let test_pred () =
    let z = I.zero in
    let zs = [|I.one;z;z;z|] |> C.of_array in
    let expected = [|z;z;z;z|] |> Array.map I.to_string in
    let zs = C.pred zs in
    let actual = C.to_string_array zs in
    Alcotest.(check (array string))
      "pred 0 with 4 digits"
      expected actual

  let test_pred_rollover () =
    let z = I.zero in
    let zs = [|z;z;z;z|] |> C.of_array in
    let m = I.max_int in
    let expected = [|m;m;m;m|] |> Array.map I.to_string in
    let zs = C.pred zs in
    let actual = C.to_string_array zs in
    Alcotest.(check (array string))
      "pred max with 4 digits"
      expected actual

  let test_failure_0 () =
    let zs = [||] in
    Alcotest.(check_raises "no data" (Invalid_argument "No data")
                (fun () -> let _ = C.of_array zs in ()))

  let test_failure_too_big () =
    let z = I.zero in
    let zs = [|z;z;z;z;z|] in
    Alcotest.(check_raises "no data" (Invalid_argument "Too large")
                (fun () -> let _ = C.of_array zs in ()))

end

module T32_2 = Ctr_test (Randii.Threefry.UInt32_2_T)
module T64_2 = Ctr_test (Randii.Threefry.UInt64_2_T)
module T32_4 = Ctr_test (Randii.Threefry.UInt32_4_T)
module T64_4 = Ctr_test (Randii.Threefry.UInt64_4_T)

let () =
  Alcotest.run "Counter Unittests"
    [
      (
        "2x32bit pred/succ",
        [
          Alcotest.test_case "of_array" `Quick T32_2.test_of_array;
          Alcotest.test_case "2x32 bit successor" `Quick T32_2.test_succ;
          Alcotest.test_case "2x32 bit successor of max" `Quick T32_2.test_succ_rollover;
          Alcotest.test_case "2x32 bit predecessor" `Quick T32_2.test_pred;
          Alcotest.test_case "2x32 bit predecessor of max" `Quick T32_2.test_pred_rollover;
        ]
      );
      (
        "2x64bit pred/succ",
        [
          Alcotest.test_case "of_array" `Quick T64_2.test_of_array;
          Alcotest.test_case "2x64 bit successor" `Quick T64_2.test_succ;
          Alcotest.test_case "2x64 bit successor of max" `Quick T64_2.test_succ_rollover;
          Alcotest.test_case "2x64 bit predecessor" `Quick T64_2.test_pred;
          Alcotest.test_case "2x64 bit predecessor of max" `Quick T64_2.test_pred_rollover;
        ]
      );
      (
        "2x32bit failures",
        [
          Alcotest.test_case "no data" `Quick T32_2.test_failure_0;
          Alcotest.test_case "too big" `Quick T32_2.test_failure_too_big;
        ]
      );
      (
        "2x64bit failures",
        [
          Alcotest.test_case "no data" `Quick T64_2.test_failure_0;
          Alcotest.test_case "too big" `Quick T64_2.test_failure_too_big;
        ]
      );
      (
        "4x32bit pred/succ",
        [
          Alcotest.test_case "of_array" `Quick T32_4.test_of_array;
          Alcotest.test_case "4x32 bit successor" `Quick T32_4.test_succ;
          Alcotest.test_case "4x32 bit successor of max" `Quick T32_4.test_succ_rollover;
          Alcotest.test_case "4x32 bit predecessor" `Quick T32_4.test_pred;
          Alcotest.test_case "4x32 bit predecessor of max" `Quick T32_4.test_pred_rollover;
        ]
      );
      (
        "4x64bit pred/succ",
        [
          Alcotest.test_case "of_array" `Quick T64_4.test_of_array;
          Alcotest.test_case "4x64 bit successor" `Quick T64_4.test_succ;
          Alcotest.test_case "4x64 bit successor of max" `Quick T64_4.test_succ_rollover;
          Alcotest.test_case "4x64 bit predecessor" `Quick T64_4.test_pred;
          Alcotest.test_case "4x64 bit predecessor of max" `Quick T64_4.test_pred_rollover;
        ]
      );
      (
        "4x32bit failures",
        [
          Alcotest.test_case "no data" `Quick T32_4.test_failure_0;
          Alcotest.test_case "too big" `Quick T32_4.test_failure_too_big;
        ]
      );
      (
        "4x64bit failures",
        [
          Alcotest.test_case "no data" `Quick T64_4.test_failure_0;
          Alcotest.test_case "too big" `Quick T64_4.test_failure_too_big;
        ]
      );
    ]
