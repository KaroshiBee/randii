(* kat - Known Answer Test
 * from random123/tests/kat_vectors
   threefry4x64 13 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 4071fabee1dc8e05 02ed3113695c9c62 397311b5b89f9d49 e21292c3258024bc
   threefry4x64 13 ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff 7eaed935479722b5 90994358c429f31c 496381083e07a75b 627ed0d746821121
   threefry4x64 13 243f6a8885a308d3 13198a2e03707344 a4093822299f31d0 082efa98ec4e6c89 452821e638d01377 be5466cf34e90c6c c0ac29b7c97c50dd 3f84d5b5b5470917 4361288ef9c1900c 8717291521782833 0d19db18c20cf47e a0b41d63ac8581e5
   threefry4x64 20 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000   09218ebde6c85537 55941f5266d86105 4bd25e16282434dc ee29ec846bd2e40b
   threefry4x64 20 ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff 29c24097942bba1b 0371bbfb0f6f4e11 3c231ffa33f83a1c cd29113fde32d168
   threefry4x64 20 243f6a8885a308d3 13198a2e03707344 a4093822299f31d0 082efa98ec4e6c89 452821e638d01377 be5466cf34e90c6c be5466cf34e90c6c c0ac29b7c97c50dd   a7e8fde591651bd9 baafd0c30138319b 84a5c1a729e685b9 901d406ccebc1ba4
   threefry4x64 72 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 94eeea8b1f2ada84 adf103313eae6670 952419a1f4b16d53 d83f13e63c9f6b11
   threefry4x64 72 ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffff 11518c034bc1ff4c 193f10b8bcdcc9f7 d024229cb58f20d8 563ed6e48e05183f
   threefry4x64 72 243f6a8885a308d3 13198a2e03707344 a4093822299f31d0 082efa98ec4e6c89 452821e638d01377 be5466cf34e90c6c be5466cf34e90c6c c0ac29b7c97c50dd acf412ccaa3b2270 c9e99bd53f2e9173 43dad469dc825948 fbb19d06c8a2b4dc
 * *)
module U = Randii.Threefry.UInt64_4_T
module R = Randii.Threefry.Make_threefry4xW_TEST(U)
module I = Unsigned.UInt64

(* The tests *)
let test_13_rounds_0000 () =
  let expected = [|"0x4071fabee1dc8e05"; "0x02ed3113695c9c62"; "0x397311b5b89f9d49"; "0xe21292c3258024bc"|]
                 |> Array.map Z.of_string (*reads hex from kat_vectors*)
                 |> Array.map Z.to_string (*outputs as big integer string*)
  in
  let actual = let z = I.zero in
    R.rand_R 13 [|z;z;z;z|] [|z;z;z;z|]
    |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 13 rounds with zeros input"
    expected actual

let test_13_rounds_ffff () =
  let expected = [|"0x7eaed935479722b5"; "0x90994358c429f31c"; "0x496381083e07a75b"; "0x627ed0d746821121"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual = let z = I.(zero |> pred) in
    R.rand_R 13 [|z;z;z;z|] [|z;z;z;z|]
    |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 13 rounds with max input"
    expected actual

let test_13_rounds_pi () =
  let expected = [|"0x4361288ef9c1900c"; "0x8717291521782833"; "0x0d19db18c20cf47e"; "0xa0b41d63ac8581e5"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let ctr = [|"0x243f6a8885a308d3"; "0x13198a2e03707344"; "0xa4093822299f31d0"; "0x082efa98ec4e6c89"; |]
              |> Array.map Z.of_string
              |> Array.map Z.to_string
              |> Array.map I.of_string
    in
    let key = [|"0x452821e638d01377"; "0xbe5466cf34e90c6c"; "0xc0ac29b7c97c50dd"; "0x3f84d5b5b5470917" |]
              |> Array.map Z.of_string
              |> Array.map Z.to_string
              |> Array.map I.of_string
    in
    R.rand_R 13 [|ctr.(0);ctr.(1);ctr.(2);ctr.(3)|] [|key.(0);key.(1);key.(2);key.(3)|]
    |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 13 rounds with pi digits input"
    expected actual

let test_20_rounds_0000 () =
  let expected = [|"0x09218ebde6c85537"; "0x55941f5266d86105"; "0x4bd25e16282434dc"; "0xee29ec846bd2e40b"|]
                 |> Array.map Z.of_string (*reads hex from kat_vectors*)
                 |> Array.map Z.to_string (*outputs as big integer string*)
  in
  let actual = let z = I.zero in
    R.rand_R 20 [|z;z;z;z|] [|z;z;z;z|]
    |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 20 rounds with zeros input"
    expected actual

let test_20_rounds_ffff () =
  let expected = [|"0x29c24097942bba1b"; "0x0371bbfb0f6f4e11"; "0x3c231ffa33f83a1c"; "0xcd29113fde32d168"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual = let z = I.(zero |> pred) in
    R.rand_R 20 [|z;z;z;z|] [|z;z;z;z|]
    |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 20 rounds with max input"
    expected actual

let test_20_rounds_pi () =
  let expected = [|"0xa7e8fde591651bd9"; "0xbaafd0c30138319b"; "0x84a5c1a729e685b9"; "0x901d406ccebc1ba4"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let ctr = [|"0x243f6a8885a308d3"; "0x13198a2e03707344"; "0xa4093822299f31d0"; "0x082efa98ec4e6c89"; |]
              |> Array.map Z.of_string
              |> Array.map Z.to_string
              |> Array.map I.of_string
    in
    let key = [|"0x452821e638d01377"; "0xbe5466cf34e90c6c"; "0xbe5466cf34e90c6c"; "0xc0ac29b7c97c50dd" |]
              |> Array.map Z.of_string
              |> Array.map Z.to_string
              |> Array.map I.of_string
    in
    R.rand_R 20 [|ctr.(0);ctr.(1);ctr.(2);ctr.(3)|] [|key.(0);key.(1);key.(2);key.(3)|]
    |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 20 rounds with pi digits input"
    expected actual
(* threefry4x64 20 243f6a8885a308d3 13198a2e03707344 a4093822299f31d0 082efa98ec4e6c89   0000000000000001 0000000000000000 0000000000000000 0000000000000000   7bbbcbf0f617f950 8eda8917ee3a6a15 56bf5df8bfd1060e df7a6d2bf0396704
*)
let test_20_rounds_pi_key_zero () =
  let expected = [|"0x7bbbcbf0f617f950"; "0x8eda8917ee3a6a15"; "0x56bf5df8bfd1060e"; "0xdf7a6d2bf0396704"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let ctr = [|"0x243f6a8885a308d3"; "0x13198a2e03707344"; "0xa4093822299f31d0"; "0x082efa98ec4e6c89"; |]
              |> Array.map Z.of_string
              |> Array.map Z.to_string
              |> Array.map I.of_string
    in
    let z = I.zero in
    let x = "0x0000000000000001" |> Z.of_string |> Z.to_string |> I.of_string in
    let key = [|x; z; z; z|] in
    R.rand_R 20 [|ctr.(0);ctr.(1);ctr.(2);ctr.(3)|] [|key.(0); key.(1); key.(2); key.(3)|]
    |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 20 rounds with pi digits input and key = 0s"
    expected actual

let test_72_rounds_0000 () =
  let expected = [|"0x94eeea8b1f2ada84"; "0xadf103313eae6670"; "0x952419a1f4b16d53"; "0xd83f13e63c9f6b11"|]
                 |> Array.map Z.of_string (*reads hex from kat_vectors*)
                 |> Array.map Z.to_string (*outputs as big integer string*)
  in
  let actual = let z = I.zero in
    R.rand_R 72 [|z;z;z;z|] [|z;z;z;z|]
    |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 72 rounds with zeros input"
    expected actual

let test_72_rounds_ffff () =
  let expected = [|"0x11518c034bc1ff4c"; "0x193f10b8bcdcc9f7"; "0xd024229cb58f20d8"; "0x563ed6e48e05183f"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual = let z = I.(zero |> pred) in
    R.rand_R 72 [|z;z;z;z|] [|z;z;z;z|]
    |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 72 rounds with max input"
    expected actual

let test_72_rounds_pi () =
  let expected = [|"0xacf412ccaa3b2270"; "0xc9e99bd53f2e9173"; "0x43dad469dc825948"; "0xfbb19d06c8a2b4dc"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let ctr = [|"0x243f6a8885a308d3"; "0x13198a2e03707344"; "0xa4093822299f31d0"; "0x082efa98ec4e6c89"; |]
              |> Array.map Z.of_string
              |> Array.map Z.to_string
              |> Array.map I.of_string
    in
    let key = [|"0x452821e638d01377"; "0xbe5466cf34e90c6c"; "0xbe5466cf34e90c6c"; "0xc0ac29b7c97c50dd" |]
              |> Array.map Z.of_string
              |> Array.map Z.to_string
              |> Array.map I.of_string
    in
    R.rand_R 72 [|ctr.(0);ctr.(1);ctr.(2);ctr.(3)|] [|key.(0);key.(1);key.(2);key.(3)|]
    |> Array.map I.to_string in
  Alcotest.(check (array string))
    "kat for 72 rounds with pi digits input"
    expected actual


let test_skein_ks_parity () =
  let expected = "2004413935125273122" in
  let actual = U.skein_ks_parity |> Unsigned.UInt64.to_string in
  Alcotest.(check string)
    "skein ks parity test"
    expected actual

(* Run it *)
let () =
  Alcotest.run "Threefry4x64 Known Answers"
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
          Alcotest.test_case "pi 0000" `Quick test_20_rounds_pi_key_zero;
        ] );
      ( "kat72",
        [
          Alcotest.test_case "0000" `Quick test_72_rounds_0000;
          Alcotest.test_case "ffff" `Quick test_72_rounds_ffff;
          Alcotest.test_case "pi" `Quick test_72_rounds_pi;
        ] );
      ( "skein_ks_parity",
        [
          Alcotest.test_case "skein ks test" `Quick test_skein_ks_parity;
        ]
      );
    ]
