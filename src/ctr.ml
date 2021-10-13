module type T = sig
  type t
  val to_string : t -> string
  val equal : t -> t -> bool
  val succ : t -> t
  val pred : t -> t
  val zero : t
  val one : t
  val max_int : t
end

module type CTR = sig
  type el
  type t
  val of_array : el array -> t
  val to_array : t -> el array
  val to_string_array : t -> string array
  val succ : t -> unit
  val pred : t -> unit
end

type digits = | Two | Four

module Ctr (U:T) : (CTR with type el := U.t)= struct
  type el = U.t
  type t = {
    data: el array;
    digits: digits
  }

  let of_array data =
    match Array.length data with
    | 0 -> raise (Invalid_argument "No data")
    | 1 -> {data=Array.(append (copy data) [| U.zero |]); digits=Two}
    | 2 -> {data=Array.copy data; digits=Two}
    | 3 -> {data=Array.(append (copy data) [| U.zero |]); digits=Four}
    | 4 -> {data=Array.copy data; digits=Four}
    | _ -> raise (Invalid_argument "Too large")
  let to_array {data; _} = Array.copy data
  let to_string_array t = to_array t |> Array.map U.to_string

  let rec aux_i digit f sentinal {data; digits} =
    let n = Array.length data in
    if digit == n then () else (
      let d = f data.(digit) in
      match (U.equal sentinal d) with
      | false -> data.(digit) <- d;
      | true -> data.(digit) <- d; aux_i (digit+1) f sentinal {data; digits}
    )

  let succ = aux_i 0 U.succ U.zero
  let pred = aux_i 0 U.pred U.max_int

end

module Ctr_32 = Ctr(Unsigned.UInt32)
module Ctr_64 = Ctr(Unsigned.UInt64)
