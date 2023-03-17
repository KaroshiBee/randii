(* rotation amounts *)
type 'num t = 'num array

let make ~to_num i_0 i_1 i_2 i_3 i_4 i_5 i_6 i_7 = [|
  to_num i_0;
  to_num i_1;
  to_num i_2;
  to_num i_3;
  to_num i_4;
  to_num i_5;
  to_num i_6;
  to_num i_7;
|]

let zeros ~to_num =
  let z = to_num 0 in [|z;z;z;z;z;z;z;z;|]

let i t ~at = t.(at)
