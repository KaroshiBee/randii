(* // Assumptions
 * // rand() in [0, RAND_MAX]
 * // n in (0, RAND_MAX]
 *
 * int x;
 *
 * // Keep searching for an x in a range divisible by n
 * do {
 *     x = rand();
 * } while (x >= RAND_MAX - (RAND_MAX % n))
 *
 * x %= n; *)

module Make_uniform2xW (U:Threefry.T2) = struct

  module Ctr = Ctr.Make_ctr(U)
  module Rng = Threefry.Make_threefry2xW(U)

  let limit n = U.(sub max_int (rem max_int n))

  let discrete ~(upper:U.t) ctr key =
    if U.equal U.zero upper then U.zero else
      match (Ctr.digits ctr, Ctr.digits key) with
      | (Two, Two) -> (
          try
            Rng.rand (Ctr.to_array ctr) (Ctr.to_array key)
            |> Array.to_list
            |> List.find_opt (fun x -> x <= limit upper)
            |> Option.map (fun x -> U.rem x upper)
            |> Option.get
          with
          | _ -> raise (Invalid_argument "Bad ctr/key pair for given ~upper")
        )
      | _ -> raise (Invalid_argument "Need two digit ctr/key")


end

module Make_uniform4xW (U:Threefry.T4) = struct

  module Ctr = Ctr.Make_ctr(U)
  module Rng = Threefry.Make_threefry4xW(U)

  let limit n = U.(sub max_int (rem max_int n))

  let discrete ~(upper:U.t) ctr key =
    if U.equal U.zero upper then U.zero else
      match (Ctr.digits ctr, Ctr.digits key) with
      | (Four, Four) -> (
          try
            Rng.rand (Ctr.to_array ctr) (Ctr.to_array key)
            |> Array.to_list
            |> List.find_opt (fun x -> x <= limit upper)
            |> Option.map (fun x -> U.rem x upper)
            |> Option.get
          with
          | _ -> raise (Invalid_argument "Bad ctr/key pair for given ~upper")
        )
      | _ -> raise (Invalid_argument "Need four digit ctr/key")


end


module Uniform = Make_uniform4xW(Threefry.UInt64_4_T)
let uniform = Uniform.discrete
