(* kat - Known Answer Test
 * from random123/tests/kat_vectors
   threefry2x64 13 0000000000000000 0000000000000000 0000000000000000 0000000000000000 f167b032c3b480bd e91f9fee4b7a6fb5
   threefry2x64 13 ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ccdec5c917a874b1 4df53abca26ceb01
   threefry2x64 13 243f6a8885a308d3 13198a2e03707344 a4093822299f31d0 082efa98ec4e6c89 c3aac71561042993 3fe7ae8801aff316
   threefry2x64 20 0000000000000000 0000000000000000 0000000000000000 0000000000000000   c2b6e3a8c2c69865 6f81ed42f350084d
   threefry2x64 20 ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff   e02cb7c4d95d277a d06633d0893b8b68
   threefry2x64 20 243f6a8885a308d3 13198a2e03707344 a4093822299f31d0 082efa98ec4e6c89   263c7d30bb0f0af1 56be8361d3311526
   threefry2x64 32 0000000000000000 0000000000000000 0000000000000000 0000000000000000 38ba854d7f13cfb3 d02fca729d54fadc
   threefry2x64 32 ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff 6b532f4f6e288646 0388f1ec135ee18e
   threefry2x64 32 243f6a8885a308d3 13198a2e03707344 a4093822299f31d0 082efa98ec4e6c89 dad492f32efbd0c4 b6d7d0cd1f193e84
*)
module R = Randii.Threefry.Threefry2x64_TEST
module I = Unsigned.UInt64

(* The tests *)
let test_13_rounds_0000 () =
  let expected = [|"0xf167b032c3b480bd"; "0xe91f9fee4b7a6fb5"|]
                 |> Array.map Z.of_string (*reads hex from kat_vectors*)
                 |> Array.map Z.to_string (*outputs as big integer string*)
  in
  let actual = let z = I.zero in
    R.rand_R 13 [|z;z|] [|z;z|] |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 13 rounds with zeros input"
    expected actual

let test_13_rounds_ffff () =
  let expected = [|"0xccdec5c917a874b1"; "0x4df53abca26ceb01"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual = let z = I.(zero |> pred) in
    R.rand_R 13 [|z;z|] [|z;z|] |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 13 rounds with max input"
    expected actual

let test_13_rounds_pi () =
  let expected = [|"0xc3aac71561042993"; "0x3fe7ae8801aff316"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual = let z = [|"0x243f6a8885a308d3"; "0x13198a2e03707344"; "0xa4093822299f31d0"; "0x082efa98ec4e6c89"; |]
                       |> Array.map Z.of_string
                       |> Array.map Z.to_string
                       |> Array.map I.of_string
    in
    R.rand_R 13 [|z.(0);z.(1)|] [|z.(2);z.(3)|] |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 13 rounds with pi digits input"
    expected actual

let test_20_rounds_0000 () =
  let expected = [|"0xc2b6e3a8c2c69865"; "0x6f81ed42f350084d"|]
                 |> Array.map Z.of_string (*reads hex from kat_vectors*)
                 |> Array.map Z.to_string (*outputs as big integer string*)
  in
  let actual = let z = I.zero in
    R.rand_R 20 [|z;z|] [|z;z|] |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 20 rounds with zeros input"
    expected actual

let test_20_rounds_ffff () =
  let expected = [|"0xe02cb7c4d95d277a"; "0xd06633d0893b8b68"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual = let z = I.(zero |> pred) in
    R.rand_R 20 [|z;z|] [|z;z|] |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 20 rounds with max input"
    expected actual

let test_20_rounds_pi () =
  let expected = [|"0x263c7d30bb0f0af1"; "0x56be8361d3311526"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual = let z = [|"0x243f6a8885a308d3"; "0x13198a2e03707344"; "0xa4093822299f31d0"; "0x082efa98ec4e6c89"; |]
                       |> Array.map Z.of_string
                       |> Array.map Z.to_string
                       |> Array.map I.of_string
    in
    R.rand_R 20 [|z.(0);z.(1)|] [|z.(2);z.(3)|] |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 20 rounds with pi digits input"
    expected actual


let test_32_rounds_0000 () =
  let expected = [|"0x38ba854d7f13cfb3"; "0xd02fca729d54fadc"|]
                 |> Array.map Z.of_string (*reads hex from kat_vectors*)
                 |> Array.map Z.to_string (*outputs as big integer string*)
  in
  let actual = let z = I.zero in
    R.rand_R 32 [|z;z|] [|z;z|] |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 32 rounds with zeros input"
    expected actual

let test_32_rounds_ffff () =
  let expected = [|"0x6b532f4f6e288646"; "0x0388f1ec135ee18e"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual = let z = I.(zero |> pred) in
    R.rand_R 32 [|z;z|] [|z;z|] |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 32 rounds with max input"
    expected actual

let test_32_rounds_pi () =
  let expected = [|"0xdad492f32efbd0c4"; "0xb6d7d0cd1f193e84"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual = let z = [|"0x243f6a8885a308d3"; "0x13198a2e03707344"; "0xa4093822299f31d0"; "0x082efa98ec4e6c89"; |]
                       |> Array.map Z.of_string
                       |> Array.map Z.to_string
                       |> Array.map I.of_string
    in
    R.rand_R 32 [|z.(0);z.(1)|] [|z.(2);z.(3)|] |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 32 rounds with pi digits input"
    expected actual

(* Run it *)
let () =
  Alcotest.run "Utils"
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
