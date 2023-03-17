module R2x32 = Cbrn.Threefry.Gen_2_32
module R2x64 = Cbrn.Threefry.Gen_2_64
module R4x32 = Cbrn.Threefry.Gen_4_32
module R4x64 = Cbrn.Threefry.Gen_4_64

module I32 = Unsigned.UInt32
module I64 = Unsigned.UInt64

let empty : string array = [||]

let test_failure_2x32 () =
  let open R2x32 in
  let z = "0" in
  let zs = [|z;z|] |> of_string_array in
  let actual = rand ~rounds:100 ~key:zs ~ctr:zs () |> to_string_array in
  Alcotest.(check (array string) "failure 2x32" actual empty)

let test_failure_4x32 () =
  let open R4x32 in
  let z = "0" in
  let zs = [|z;z;z;z|] |> of_string_array in
  let actual = rand ~rounds:100 ~key:zs ~ctr:zs () |> to_string_array in
  Alcotest.(check (array string) "failure 4x32" actual empty)

let test_failure_2x64 () =
  let open R2x64 in
  let z = "0" in
  let zs = [|z;z|] |> of_string_array in
  let actual = rand ~rounds:100 ~key:zs ~ctr:zs () |> to_string_array in
  Alcotest.(check (array string) "failure 2x64" actual empty)

let test_failure_4x64 () =
  let open R4x64 in
  let z = "0" in
  let zs = [|z;z;z;z|] |> of_string_array in
  let actual = rand ~rounds:100 ~key:zs ~ctr:zs () |> to_string_array in
  Alcotest.(check (array string) "failure 4x64" actual empty)

let test_threefry2x32 () =
  let open R2x32 in
  let expected = [|"0x6b200159"; "0x99ba4efe"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let z = "0" in
    let zs = [|z;z|] |> of_string_array in
    rand ~key:zs ~ctr:zs () |> to_string_array
  in
  Alcotest.(check (array string))
    "Threefry2x32 with zeros input"
    expected actual

let test_threefry2x64 () =
  let open R2x64 in
  let expected = [|"0xc2b6e3a8c2c69865"; "0x6f81ed42f350084d"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let z = "0" in
    let zs = [|z;z|] |> of_string_array in
    rand ~key:zs ~ctr:zs () |> to_string_array
  in
  Alcotest.(check (array string))
    "Threefry2x64 with zeros input"
    expected actual

let test_threefry4x32 () =
  let open R4x32 in
  let expected = [|"0x9c6ca96a"; "0xe17eae66"; "0xfc10ecd4"; "0x5256a7d8"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let z = "0" in
    let zs = [|z;z;z;z|] |> of_string_array in
    rand ~key:zs ~ctr:zs () |> to_string_array
  in
  Alcotest.(check (array string))
    "Threefry4x32 with zeros input"
    expected actual

let test_threefry4x64 () =
  let open R4x64 in
  let expected = [|"0x09218ebde6c85537"; "0x55941f5266d86105"; "0x4bd25e16282434dc"; "0xee29ec846bd2e40b"|]
                 |> Array.map Z.of_string
                 |> Array.map Z.to_string
  in
  let actual =
    let z = "0" in
    let zs = [|z;z;z;z|] |> of_string_array in
    rand ~key:zs ~ctr:zs () |> to_string_array
  in
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
