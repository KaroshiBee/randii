(* kat - Known Answer Test
 * from random123/tests/kat_vectors
 * threefry4x32 13 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 531c7e4f 39491ee5 2c855a92 3d6abf9a
   threefry4x32 13 ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff c4189358 1c9cc83a d5881c67 6a0a89e0
   threefry4x32 13 243f6a88 85a308d3 13198a2e 03707344 a4093822 299f31d0 082efa98 ec4e6c89 4aa71d8f 734738c2 431fc6a8 ae6debf1
   threefry4x32 20 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000   9c6ca96a e17eae66 fc10ecd4 5256a7d8
   threefry4x32 20 ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff   2a881696 57012287 f6c7446e a16a6732
   threefry4x32 20 243f6a88 85a308d3 13198a2e 03707344 a4093822 299f31d0 082efa98 ec4e6c89   59cd1dbb b8879579 86b5d00c ac8b6d84
   threefry4x32 72 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 93171da6 9220326d b392b7b1 ff58a002
   threefry4x32 72 ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff 60743f3d 9961e684 aab21c34 8c65fb7d
   threefry4x32 72 243f6a88 85a308d3 13198a2e 03707344 a4093822 299f31d0 082efa98 ec4e6c89 09930adf 7f27bd55 9ed68ce1 97f803f6

 * *)
module U = Randii.Threefry.UInt32_4_T
module R = Randii.Threefry.Make_threefry4xW_TEST(U)
module I = Unsigned.UInt32

(* The tests *)
let test_13_rounds_0000 () =
  let expected = [|0x531c7e4f; 0x39491ee5; 0x2c855a92; 0x3d6abf9a|] in
  let actual = let z = I.zero in
    R.rand_R 13 [|z;z;z;z|] [|z;z;z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 13 rounds with zeros input"
    expected actual

let test_13_rounds_ffff () =
  let expected = [|0xc4189358; 0x1c9cc83a; 0xd5881c67; 0x6a0a89e0|] in
  let actual = let z = 0xffffffff |> I.of_int in
    R.rand_R 13 [|z;z;z;z|] [|z;z;z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 13 rounds with max input"
    expected actual

let test_13_rounds_pi () =
  let expected = [|0x4aa71d8f; 0x734738c2; 0x431fc6a8; 0xae6debf1 |] in
  let actual =
    let z0 = [|0x243f6a88; 0x85a308d3; 0x13198a2e; 0x03707344 |] |> Array.map I.of_int in
    let z1 = [|0xa4093822; 0x299f31d0; 0x082efa98; 0xec4e6c89 |] |> Array.map I.of_int in
    R.rand_R 13 [|z0.(0);z0.(1); z0.(2);z0.(3)|] [|z1.(0);z1.(1); z1.(2);z1.(3)|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 13 rounds with pi digits input"
    expected actual

let test_20_rounds_0000 () =
  let expected = [|0x9c6ca96a; 0xe17eae66; 0xfc10ecd4; 0x5256a7d8|] in
  let actual = let z = I.zero in
    R.rand_R 20 [|z;z;z;z|] [|z;z;z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 20 rounds with zeros input"
    expected actual

let test_20_rounds_ffff () =
  let expected = [|0x2a881696; 0x57012287; 0xf6c7446e; 0xa16a6732|] in
  let actual = let z = 0xffffffff |> I.of_int in
    R.rand_R 20 [|z;z;z;z|] [|z;z;z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 20 rounds with max input"
    expected actual

let test_20_rounds_pi () =
  let expected = [|0x59cd1dbb; 0xb8879579; 0x86b5d00c; 0xac8b6d84 |] in
  let actual =
    let z0 = [|0x243f6a88; 0x85a308d3; 0x13198a2e; 0x03707344 |] |> Array.map I.of_int in
    let z1 = [|0xa4093822; 0x299f31d0; 0x082efa98; 0xec4e6c89 |] |> Array.map I.of_int in
    R.rand_R 20 [|z0.(0);z0.(1); z0.(2);z0.(3)|] [|z1.(0);z1.(1); z1.(2);z1.(3)|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 20 rounds with pi digits input"
    expected actual

let test_72_rounds_0000 () =
  let expected = [|0x93171da6; 0x9220326d; 0xb392b7b1; 0xff58a002|] in
  let actual = let z = I.zero in
    R.rand_R 72 [|z;z;z;z|] [|z;z;z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 72 rounds with zeros input"
    expected actual

let test_72_rounds_ffff () =
  let expected = [|0x60743f3d; 0x9961e684; 0xaab21c34; 0x8c65fb7d|] in
  let actual = let z = 0xffffffff |> I.of_int in
    R.rand_R 72 [|z;z;z;z|] [|z;z;z;z|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 72 rounds with max input"
    expected actual

let test_72_rounds_pi () =
  let expected = [|0x09930adf; 0x7f27bd55; 0x9ed68ce1; 0x97f803f6 |] in
  let actual =
    let z0 = [|0x243f6a88; 0x85a308d3; 0x13198a2e; 0x03707344 |] |> Array.map I.of_int in
    let z1 = [|0xa4093822; 0x299f31d0; 0x082efa98; 0xec4e6c89 |] |> Array.map I.of_int in
    R.rand_R 72 [|z0.(0);z0.(1); z0.(2);z0.(3)|] [|z1.(0);z1.(1); z1.(2);z1.(3)|] |> Array.map I.to_int in
  Alcotest.(check (array int))
    "kat for 72 rounds with pi digits input"
    expected actual


(* Run it *)
let () =
  Alcotest.run "Threefry4x32 Known Answers"
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
      ( "kat72",
        [
          Alcotest.test_case "0000" `Quick test_72_rounds_0000;
          Alcotest.test_case "ffff" `Quick test_72_rounds_ffff;
          Alcotest.test_case "pi" `Quick test_72_rounds_pi;
        ] );
    ]
