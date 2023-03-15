type two_digits
type four_digits

module Consts = struct
  type 'a t = {
    (* rotation amounts *)
    i_0: 'a;
    i_1: 'a;
    i_2: 'a;
    i_3: 'a;
    i_4: 'a;
    i_5: 'a;
    i_6: 'a;
    i_7: 'a;
  }

  let make f i_0 i_1 i_2 i_3 i_4 i_5 i_6 i_7 = {
    i_0=f i_0;
    i_1=f i_1;
    i_2=f i_2;
    i_3=f i_3;
    i_4=f i_4;
    i_5=f i_5;
    i_6=f i_6;
    i_7=f i_7;
  }

  let zeros f = {
    i_0=f 0;
    i_1=f 0;
    i_2=f 0;
    i_3=f 0;
    i_4=f 0;
    i_5=f 0;
    i_6=f 0;
    i_7=f 0;
  }

end


(* factor out dependence on UInt32 / UInt64 *)
module type T = sig
  type digits
  type t (* UInt32 or UInt64 *)
  val of_int : int -> t
  val to_int : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val succ : t -> t
  val pred : t -> t
  val zero : t
  val one : t
  val max_int : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val rem : t -> t -> t
  val logxor : t -> t -> t

  val default_rounds : int
  val skein_ks_parity : t
  val rotations_0 : t Consts.t
  val rotations_1 : t Consts.t

  val rotL : t -> t -> t

end

module type T2 = sig
  include T
  type digits = two_digits
end

module type T4 = sig
  include T
  type digits = four_digits
end

type digits = | Two | Four

module type CTR = sig
  type el
  type t
  val of_string_array : string array -> (t, Errors.t) Result.t
  val to_string_array : t -> string array
  val copy : t -> t
  val succ : t -> t
  val pred : t -> t
  val digits : t -> digits
  val data : t -> el array
end

(* NOTE also that this implementation swaps around the args key/ctr
   as compared to the original C implementation *)

module type RAND_T = sig
  type t
  val rand : key:t -> ctr:t -> t
end

(* expose rand_R for testing purposes *)
module type RAND_TEST_T = sig
  include RAND_T
  val rand_R : rounds:int -> key:t -> ctr:t -> t
end



