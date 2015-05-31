open Owee_buf

type header
val read_chunk : cursor -> (header * cursor) option

type state = {
  mutable address        : int;
  mutable filename       : string;
  mutable file           : int;
  mutable line           : int;
  mutable col            : int;
  mutable is_statement   : bool;
  mutable basic_block    : bool;
  mutable end_sequence   : bool;
  mutable prologue_end   : bool;
  mutable epilogue_begin : bool;
  mutable isa            : int;
  mutable discriminator  : int;
}

val get_filename : header -> state -> string option
val interpret : header -> cursor ->
  (header -> state -> 'a -> 'a) -> 'a -> 'a
