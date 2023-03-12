module R2x32 = Randii.Threefry.Threefry2x32
module R2x64 = Randii.Threefry.Threefry2x64
module R4x32 = Randii.Threefry.Threefry4x32
module R4x64 = Randii.Threefry.Threefry4x64

module I32 = Unsigned.UInt32
module I64 = Unsigned.UInt64

module R2x32_T = Randii.Threefry.Make_threefry2xW_TEST(Randii.Threefry.UInt32_2_T)
module R4x32_T = Randii.Threefry.Make_threefry4xW_TEST(Randii.Threefry.UInt32_4_T)
module R2x64_T = Randii.Threefry.Make_threefry2xW_TEST(Randii.Threefry.UInt64_2_T)
module R4x64_T = Randii.Threefry.Make_threefry4xW_TEST(Randii.Threefry.UInt64_4_T)

let test_failure_2x32 () =
  let z = I32.zero in
  let zs = [|z;z|] in
  Alcotest.(check_raises "nrounds too big" (Failure "number rounds must be <= 32")
              (fun () -> let _ = R2x32_T.rand_R 100 zs zs in ()))

let test_failure_4x32 () =
  let z = I32.zero in
  let zs = [|z;z;z;z|] in
  Alcotest.(check_raises "nrounds too big" (Failure "number rounds must be <= 72")
              (fun () -> let _ = R4x32_T.rand_R 100 zs zs in ()))

let test_failure_2x64 () =
  let z = I64.zero in
  let zs = [|z;z|] in
  Alcotest.(check_raises "nrounds too big" (Failure "number rounds must be <= 32")
              (fun () -> let _ = R2x64_T.rand_R 100 zs zs in ()))

let test_failure_4x64 () =
  let z = I64.zero in
  let zs = [|z;z;z;z|] in
  Alcotest.(check_raises "nrounds too big" (Failure "number rounds must be <= 72")
              (fun () -> let _ = R4x64_T.rand_R 100 zs zs in ()))

let test_threefry2x32 () =
  let expected = [|"0x6b200159"; "0x99ba4efe"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let z = I32.zero in
    let zs = [|z;z|] in
    R2x32.rand zs zs |> Array.map I32.to_string in
  Alcotest.(check (array string))
    "Threefry2x32 with zeros input"
    expected actual

let test_threefry2x64 () =
  let expected = [|"0xc2b6e3a8c2c69865"; "0x6f81ed42f350084d"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let z = I64.zero in
    let zs = [|z;z|] in
    R2x64.rand zs zs |> Array.map I64.to_string in
  Alcotest.(check (array string))
    "Threefry2x64 with zeros input"
    expected actual

let test_threefry4x32 () =
  let expected = [|"0x9c6ca96a"; "0xe17eae66"; "0xfc10ecd4"; "0x5256a7d8"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let z = I32.zero in
    let zs = [|z;z;z;z|] in
    R4x32.rand zs zs |> Array.map I32.to_string in
  Alcotest.(check (array string))
    "Threefry4x32 with zeros input"
    expected actual

let test_threefry4x64 () =
  let expected = [|"0x09218ebde6c85537"; "0x55941f5266d86105"; "0x4bd25e16282434dc"; "0xee29ec846bd2e40b"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let z = I64.zero in
    let zs = [|z;z;z;z|] in
    R4x64.rand zs zs |> Array.map I64.to_string in
  Alcotest.(check (array string))
    "Threefry4x64 with zeros input"
    expected actual


let () =
  Alcotest.run "Threefry Unittests"
    [
      (
        "failures",
        [
          Alcotest.test_case "2x32 bit" `Quick test_failure_2x32;
          Alcotest.test_case "4x32 bit" `Quick test_failure_4x32;
          Alcotest.test_case "2x64 bit" `Quick test_failure_2x64;
          Alcotest.test_case "4x64 bit" `Quick test_failure_4x64;
        ]
      );
      (
        "2 digits",
        [
          Alcotest.test_case "32 bit" `Quick test_threefry2x32;
          Alcotest.test_case "64 bit" `Quick test_threefry2x64;
        ]
      );
      (
        "4 digits",
        [
          Alcotest.test_case "32 bit" `Quick test_threefry4x32;
          Alcotest.test_case "64 bit" `Quick test_threefry4x64;
        ]
      );
    ]
