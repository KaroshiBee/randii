(* kat - Known Answer Test
 * from random123/tests/kat_vectors
 * threefry2x32 13 00000000 00000000 00000000 00000000 9d1c5ec6 8bd50731
   threefry2x32 13 ffffffff ffffffff ffffffff ffffffff fd36d048 2d17272c
   threefry2x32 13 243f6a88 85a308d3 13198a2e 03707344 ba3e4725 f27d669e
   threefry2x32 20 00000000 00000000 00000000 00000000   6b200159 99ba4efe
   threefry2x32 20 ffffffff ffffffff ffffffff ffffffff   1cb996fc bb002be7
   threefry2x32 20 243f6a88 85a308d3 13198a2e 03707344   c4923a9c 483df7a0
   threefry2x32 32 00000000 00000000 00000000 00000000 cee3d47e a23dfd5c
   threefry2x32 32 ffffffff ffffffff ffffffff ffffffff 6e2fe0d0 b1b76f82
   threefry2x32 32 243f6a88 85a308d3 13198a2e 03707344 e2827716 c3c05cdf
*)
module U = Randii.Threefry.UInt32_2_T
module R = Randii.Threefry.Make_threefry2xW_TEST(U)
module I = Unsigned.UInt32

(* The tests *)
let test_13_rounds_0000 () =
  let expected = [|0x9d1c5ec6; 0x8bd50731|] in
  let actual = let z = I.zero in
    R.rand_R 13 [|z;z|] [|z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 13 rounds with zeros input"
    expected actual

let test_13_rounds_ffff () =
  let expected = [|0xfd36d048; 0x2d17272c|] in
  let actual = let z = 0xffffffff |> I.of_int in
    R.rand_R 13 [|z;z|] [|z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 13 rounds with max input"
    expected actual

let test_13_rounds_pi () =
  let expected = [|0xba3e4725; 0xf27d669e|] in
  let actual = let z = [|0x243f6a88; 0x85a308d3; 0x13198a2e; 0x03707344; |] |> Array.map I.of_int in
    R.rand_R 13 [|z.(0);z.(1)|] [|z.(2);z.(3)|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 13 rounds with pi digits input"
    expected actual

let test_20_rounds_0000 () =
  let expected = [|0x6b200159; 0x99ba4efe|] in
  let actual = let z = I.zero in
    R.rand_R 20 [|z;z|] [|z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 20 rounds with zeros input"
    expected actual

let test_20_rounds_ffff () =
  let expected = [|0x1cb996fc; 0xbb002be7|] in
  let actual = let z = 0xffffffff |> I.of_int in
    R.rand_R 20 [|z;z|] [|z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 20 rounds with max input"
    expected actual

let test_20_rounds_pi () =
  let expected = [|0xc4923a9c; 0x483df7a0|] in
  let actual = let z = [|0x243f6a88; 0x85a308d3; 0x13198a2e; 0x03707344; |] |> Array.map I.of_int in
    R.rand_R 20 [|z.(0);z.(1)|] [|z.(2);z.(3)|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 20 rounds with pi digits input"
    expected actual

let test_32_rounds_0000 () =
  let expected = [|0xcee3d47e; 0xa23dfd5c|] in
  let actual = let z = I.zero in
    R.rand_R 32 [|z;z|] [|z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 32 rounds with zeros input"
    expected actual

let test_32_rounds_ffff () =
  let expected = [|0x6e2fe0d0; 0xb1b76f82|] in
  let actual = let z = 0xffffffff |> I.of_int in
    R.rand_R 32 [|z;z|] [|z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 32 rounds with max input"
    expected actual

let test_32_rounds_pi () =
  let expected = [|0xe2827716; 0xc3c05cdf|] in
  let actual = let z = [|0x243f6a88; 0x85a308d3; 0x13198a2e; 0x03707344; |] |> Array.map I.of_int in
    R.rand_R 32 [|z.(0);z.(1)|] [|z.(2);z.(3)|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 32 rounds with pi digits input"
    expected actual

(* Run it *)
let () =
  Alcotest.run "Threefry2x32 Known Answers"
    [
      ( "kat13",
        [
          Alcotest.test_case "0000" `Quick test_13_rounds_0000;
          Alcotest.test_case "ffff" `Quick test_13_rounds_ffff;
          Alcotest.test_case "pi" `Quick test_13_rounds_pi;
        ] );
      ( "kat20",
        [
          Alcotest.test_case "0000" `Quick test_20_rounds_0000;
          Alcotest.test_case "ffff" `Quick test_20_rounds_ffff;
          Alcotest.test_case "pi" `Quick test_20_rounds_pi;
        ] );
      ( "kat32",
        [
          Alcotest.test_case "0000" `Quick test_32_rounds_0000;
          Alcotest.test_case "ffff" `Quick test_32_rounds_ffff;
          Alcotest.test_case "pi" `Quick test_32_rounds_pi;
        ] );
    ]
