(* Need unbiased transform to [0, n]
 * taken from
 * https://stackoverflow.com/questions/10984974/why-do-people-say-there-is-modulo-bias-when-using-a-random-number-generator
 * // Assumptions
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

module Maker_
    (U:Threefry.T)
    (Ctr:Ctr.CTR with type el := U.t)
    (Rng:Threefry.RAND_T with type ctr_t := U.t array and type key_t := U.t array)
= struct

  let limit n = U.(sub max_int (rem max_int n))

  let unbiased upper r =
    try
      (* find first rand less than limit
       * and do the remainder with that number
       * throws if no number is found *)
      Array.to_list r
      |> List.find_opt (fun x -> x <= limit upper)
      |> Option.map (fun x -> U.rem x upper)
      |> Option.get
    with
    | _ -> raise (Invalid_argument "Bad ctr/key pair for given ~upper")

  let discrete2 ~(upper:U.t) key ctr =
    if U.equal U.zero upper then U.zero else
      match (Ctr.digits ctr, Ctr.digits key) with
      | (Two, Two) -> (
          Rng.rand (Ctr.data key) (Ctr.data ctr)
          |> unbiased upper
        )
      | _ -> raise (Invalid_argument "Need two digit ctr/key")

  let discrete4 ~(upper:U.t) key ctr =
    if U.equal U.zero upper then U.zero else
      match (Ctr.digits ctr, Ctr.digits key) with
      | (Four, Four) -> (
          Rng.rand (Ctr.data key) (Ctr.data ctr)
          |> unbiased upper
        )
      | _ -> raise (Invalid_argument "Need four digit ctr/key")
end

module type UNIFORM_DISCRETE = sig
  type t
  type ctr
  val uniform : upper:t -> ctr -> ctr -> t
end

module Make_uniform2xW (U:Threefry.T2) = struct

  module Ctr = Ctr.Make_ctr(U)
  module Rng = Threefry.Make_threefry2xW(U)

  open Maker_(U) (Ctr) (Rng)

  let discrete = discrete2
end

module Make_uniform4xW (U:Threefry.T4) = struct

  module Ctr = Ctr.Make_ctr(U)
  module Rng = Threefry.Make_threefry4xW(U)

  open Maker_(U) (Ctr) (Rng)

  let discrete = discrete4

end


module Uniform = Make_uniform4xW(Threefry.UInt64_4_T)
let uniform = Uniform.discrete
