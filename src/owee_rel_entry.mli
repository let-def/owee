open! Owee_buf

type t

val addend : t -> i64 option
val offset : t -> u64
val info : t -> u64
val type_ : t -> u32
val symbol_index : t -> u32

val read_rela : cursor -> t
val read_rel : cursor -> t
