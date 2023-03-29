open! Owee_buf

type t = private {
  (* Section header of this relocation entry *)
  section : Owee_elf.section;
  (* Relocation entries. The ordering is preserved, meaning that the i-th element
     of this array is the i-th entry from the section. *)
  entries : Owee_rel_entry.t array;
}

(** [read_rela ~type_ buf sections] returns the list of relocations stored in the .rela
    section of the specified type.
    Returns None if there are no relocations.
*)
val read_rela : type_:[`Plt | `Dyn ] -> Owee_buf.t -> Owee_elf.section array -> t option

(** [read_rel ~type_ buf sections] returns the list of relocations stored in the .rel
    section of the specified type.
    Returns None if there are no relocations.
*)
val read_rel : type_:[`Plt | `Dyn ] -> Owee_buf.t -> Owee_elf.section array -> t option
