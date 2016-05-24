(** Low-level buffer manipulation library *)

type t =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Size of the buffer *)
(* CR mshinwell: rename to "size" *)
val dim     : t -> int

(** Minimal support for error reporting. FIXME: Improve that someday. *)
exception Invalid_format of string
val invalid_format : string -> 'a
val assert_format : bool -> string -> unit

(* Some aliases to make more explicit the nature of values being read ...
   FIXME: As a first approximation, all values are expected to fit in OCaml
   integers.
*)
type s8   = int
type u8   = int
type u16  = int
type s32  = int
type u32  = int
(* CR mshinwell: u64 should be Int64.t *)
type u64  = int (* Bye bye 32 bits. 63 bits ought to be enough for anyone. *)
type s128 = int (* Ahem, we don't expect 128 bits to really consume 128 bits *)
type u128 = int

(** A mutable cursor, pointing to an arbitrary position of a buffer *)
type cursor = {
  buffer: t;
  mutable position: int;
}

val cursor  : ?at:int -> t -> cursor
val seek    : cursor -> int -> unit
val ensure  : cursor -> int -> string -> unit
val advance : cursor -> int -> unit
val at_end  : cursor -> bool

(** [sub t len] returns a fresh cursor pointing to the
    beginning of a sub-buffer of size [len] starting from [t], and
    advances [t] by [len] *)
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

  (** [fixed_string t len] reads a string of exactly [len] bytes from [t] *)
  val fixed_string : cursor -> int -> string

  (** [zero_string msg t ?maxlen ()] reads a zero-terminated string from [t],
      stopping at the first zero or when [maxlen] is reached, if it was provided. *)
  (* CR mshinwell: delete the first argument and return an option *)
  val zero_string : string -> cursor -> ?maxlen:int -> unit -> string
end
