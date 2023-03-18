type digits_two
type digits_four
type word_32
type word_64

(* factor out dependence on UInt32 / UInt64 and word size (32/64 bit) *)
module type NUM = sig

  type digits
  type word
  type ('digits, 'word) t

  (* ctors *)
  val of_int : int -> (digits, word) t
  val of_string : string -> (digits, word) t

  (* dtors *)
  val to_int : ('digits, 'word) t -> int
  val to_string : ('digits, 'word) t -> string

  (* consts *)
  val zero : (digits, word) t
  val one : (digits, word) t
  val max_int : (digits, word) t

  val skein_ks_parity : (digits, word) t
  val rotations_0 : (digits, word) t Rotations.t
  val rotations_1 : (digits, word) t Rotations.t
  val digits : int

  (* ops *)
  val equal : ('digits, 'word) t -> ('digits, 'word) t -> bool
  val succ : ('digits, 'word) t -> ('digits, 'word) t
  val pred : ('digits, 'word) t -> ('digits, 'word) t
  val add : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t
  val sub : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t
  val rem : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t
  val logxor : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t
  val rotL : ('digits, 'word) t -> ('digits, 'word) t -> ('digits, 'word) t
end


(* NOTE also that this implementation swaps around the args key/ctr
   as compared to the original C implementation *)
module type RNG_MAKER = sig
  type digits (* sentinal type to allow for constraints *)
  module Make : functor (Num:NUM) -> sig
    val rand_R :
      of_int:(int -> (Num.digits, Num.word) Num.t) ->
      rounds:int ->
      key:(Num.digits, Num.word) Num.t array ->
      ctr:(Num.digits, Num.word) Num.t array ->
      (Num.digits, Num.word) Num.t array
  end
end

type kind =
    Rand
  | Uniform01
  | Uniform of int

module type GEN = sig
  type t
  val of_int_array : int array ->  t
  val to_int_array :  t -> int array
  val of_string_array : string array ->  t
  val to_string_array :  t -> string array

  val succ :  t ->  t
  val pred :  t ->  t

  val rand : ?rounds:int -> key:t -> ctr:t -> unit -> t
  val uniform : ?upper:int -> ?rounds:int -> key:t -> ctr:t -> unit -> int array
  val uniform01 : ?rounds:int -> key:t -> ctr:t -> unit -> float array
  val draw_from :
    rand:(?rounds:int -> key:t -> ctr:t -> unit -> t) ->
    uniform01:(?rounds:int -> key:t -> ctr:t -> unit -> float array) ->
    uniform:(?upper:int -> ?rounds:int -> key:t -> ctr:t -> unit -> int array) ->
    key:t ->
    ctr:t ->
    kind ->
    string array

  val digits : int
  val is_zero : t -> bool

end
