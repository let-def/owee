type t =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type cursor = {
  buffer: t;
  mutable position: int;
}

exception Invalid_format of string

val invalid_format : string -> 'a
val assert_format : bool -> string -> unit

type s8   = int
type u8   = int
type u16  = int
type u32  = int
type u64  = int
type s128 = int
type u128 = int

val cursor  : t -> cursor
val seek    : cursor -> int -> unit
val ensure  : cursor -> int -> string -> unit
val advance : cursor -> int -> unit
val at_end  : cursor -> bool

val dim     : t -> int
val sub     : cursor -> int -> cursor

module Read : sig
  val s8      : cursor -> s8
  val u8      : cursor -> u8
  val u16     : cursor -> u16
  val u32     : cursor -> u32
  val u32be   : cursor -> u32
  val u64     : cursor -> u64
  val uleb128 : cursor -> u128
  val sleb128 : cursor -> s128
  val fixed_string : cursor -> int -> string
  val zero_string : string -> cursor -> ?maxlen:int -> unit -> string
end
