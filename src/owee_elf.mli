open Owee_buf

(** Minimalist ELF 64 decoder *)

type identification = {
  elf_class      : u8;
  elf_data       : u8;
  elf_version    : u8;
  elf_osabi      : u8;
  elf_abiversion : u8;
}

type header = {
  e_ident     : identification;
  e_type      : u16;
  e_machine   : u16;
  e_version   : u32;
  e_entry     : u64;
  e_phoff     : u64;
  e_shoff     : u64;
  e_flags     : u32;
  e_ehsize    : u16;
  e_phentsize : u16;
  e_phnum     : u16;
  e_shentsize : u16;
  e_shnum     : u16;
  e_shstrndx  : u16;
}

type section = {
  sh_name      : u32;
  sh_type      : u32;
  sh_flags     : u64;
  sh_addr      : u64;
  sh_offset    : u64;
  sh_size      : u64;
  sh_link      : u32;
  sh_info      : u32;
  sh_addralign : u64;
  sh_entsize   : u64;
  sh_name_str  : string;
}

(** From a buffer pointing to an ELF image, [read_elf] decodes the header and
    section table. *)
val read_elf : Owee_buf.t -> header * section array

(** [section_body elf section] returns a sub-buffer with the contents of the
    [section] of the ELF image. *)
val section_body : Owee_buf.t -> section -> Owee_buf.t

(** Convenience function to find a section in the section table given its name. *)
val find_section : section array -> string -> section option
