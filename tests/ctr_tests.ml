open Randii.Types

module Ctr2_test (C:GEN) = struct

  let test_succ () =
    let z = 0 in
    let zs = [|z;z|] |> C.of_int_array in
    let expected = [|1;z|] in
    let zs = C.succ zs in
    let actual = C.to_int_array zs in
    Alcotest.(check (array int))
      "succ 0 with 2 digits"
      expected actual

  let test_pred () =
    let z = 0 in
    let zs = [|1;z|] |> C.of_int_array in
    let expected = [|z;z|] in
    let zs = C.pred zs in
    let actual = C.to_int_array zs in
    Alcotest.(check (array int))
      "pred 1 with 2 digits"
      expected actual

  let test_rollover () =
    let z = 0 in
    let zs = [|z;z|] |> C.of_int_array |> C.pred |> C.succ in
    let expected = [|z;z|] in
    let actual = C.to_int_array zs in
    Alcotest.(check (array int))
      "succ/pred over max with 2 digits"
      expected actual

  let test_failure_0 () =
    let zs = [||] in
    Alcotest.check_raises
      "Expected no data"
      (Invalid_argument "Need 2 digit ctr/key")
      (fun () -> let _ = C.of_int_array zs in ())

  let test_failure_too_big () =
    let z = 0 in
    let zs = [|z;z;z|] in
    Alcotest.check_raises
      "Expected wrong size"
      (Invalid_argument "Need 2 digit ctr/key")
      (fun () -> let _ = C.of_int_array zs in ())

end

module Ctr4_test (C:GEN) = struct

  let test_succ () =
    let z = 0 in
    let zs = [|z;z;z;z|] |> C.of_int_array in
    let expected = [|1;z;z;z|] in
    let zs = C.succ zs in
    let actual = C.to_int_array zs in
    Alcotest.(check (array int))
      "succ 0 with 4 digits"
      expected actual

  let test_pred () =
    let z = 0 in
    let zs = [|1;z;z;z|] |> C.of_int_array in
    let expected = [|z;z;z;z|] in
    let zs = C.pred zs in
    let actual = C.to_int_array zs in
    Alcotest.(check (array int))
      "pred 1 with 4 digits"
      expected actual

  let test_rollover () =
    let z = 0 in
    let zs = [|z;z;z;z|] |> C.of_int_array |> C.pred |> C.succ in
    let expected = [|z;z;z;z|] in
    let actual = C.to_int_array zs in
    Alcotest.(check (array int))
      "succ/pred over max with 4 digits"
      expected actual

  let test_failure_0 () =
    let zs = [||] in
    Alcotest.check_raises
      "Expected no data"
      (Invalid_argument "Need 4 digit ctr/key")
      (fun () -> let _ = C.of_int_array zs in ())

  let test_failure_too_big () =
    let z = 0 in
    let zs = [|z;z;z;z;z|] in
    Alcotest.check_raises
      "Expected wrong size"
      (Invalid_argument "Need 4 digit ctr/key")
      (fun () -> let _ = C.of_int_array zs in ())

end
module T32_2 = Ctr2_test (Randii.Threefry.Gen_2_32)
module T64_2 = Ctr2_test (Randii.Threefry.Gen_2_64)
module T32_4 = Ctr4_test (Randii.Threefry.Gen_4_32)
module T64_4 = Ctr4_test (Randii.Threefry.Gen_4_64)

let () =
  Alcotest.run "Counter Unittests"
    [
      (
        "2x32bit pred/succ",
        [
          (* Alcotest.test_case "of_string_array" `Quick T32_2.test_of_string_array; *)
          Alcotest.test_case "2x32 bit successor" `Quick T32_2.test_succ;
          Alcotest.test_case "2x32 bit predecessor" `Quick T32_2.test_pred;
          Alcotest.test_case "2x32 bit predecessor of max" `Quick T32_2.test_rollover;
        ]
      );
      (
        "2x64bit pred/succ",
        [
          (* Alcotest.test_case "of_string_array" `Quick T64_2.test_of_string_array; *)
          Alcotest.test_case "2x64 bit successor" `Quick T64_2.test_succ;
          Alcotest.test_case "2x64 bit predecessor" `Quick T64_2.test_pred;
          Alcotest.test_case "2x64 bit predecessor of max" `Quick T64_2.test_rollover;
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
          (* Alcotest.test_case "of_string_array" `Quick T32_4.test_of_string_array; *)
          Alcotest.test_case "4x32 bit successor" `Quick T32_4.test_succ;
          Alcotest.test_case "4x32 bit predecessor" `Quick T32_4.test_pred;
          Alcotest.test_case "4x32 bit predecessor of max" `Quick T32_4.test_rollover;
        ]
      );
      (
        "4x64bit pred/succ",
        [
          (* Alcotest.test_case "of_string_array" `Quick T64_4.test_of_string_array; *)
          Alcotest.test_case "4x64 bit successor" `Quick T64_4.test_succ;
          Alcotest.test_case "4x64 bit predecessor" `Quick T64_4.test_pred;
          Alcotest.test_case "4x64 bit predecessor of max" `Quick T64_4.test_rollover;
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
