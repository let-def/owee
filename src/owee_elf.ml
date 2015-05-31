open Owee_buf

let read_magic t =
  ensure t 4 "Magic number truncated";
  let {buffer; position} = t in
  let valid =
    buffer.{position + 0} = 0x7f &&
    buffer.{position + 1} = Char.code 'E' &&
    buffer.{position + 2} = Char.code 'L' &&
    buffer.{position + 3} = Char.code 'F'
  in
  if not valid then
    invalid_format "No ELF magic number";
  advance t 4

type identification = {
  elf_class      : u8;
  elf_data       : u8;
  elf_version    : u8;
  elf_osabi      : u8;
  elf_abiversion : u8;
}

let read_identification t =
  ensure t 12 "Identification truncated";
  let elf_class      = Read.u8 t in
  let elf_data       = Read.u8 t in
  let elf_version    = Read.u8 t in
  let elf_osabi      = Read.u8 t in
  let elf_abiversion = Read.u8 t in
  if not (Read.u8 t = 0 &&
          Read.u8 t = 0 &&
          Read.u8 t = 0 &&
          Read.u8 t = 0 &&
          Read.u8 t = 0 &&
          Read.u8 t = 0 &&
          Read.u8 t = 0)
  then
    invalid_format "Incorrect padding after identification";
  { elf_class; elf_data; elf_version;
    elf_osabi; elf_abiversion }

type header = {
  e_ident     : identification;
  e_type      : u16;
  e_machine   : u16; (* Architecture type.                 *)
  e_version   : u32; (* Object file version.               *)
  e_entry     : u64; (* Entry point virtual address.       *)
  e_phoff     : u64; (* Program header table file offset.  *)
  e_shoff     : u64; (* Section header table file offset.  *)
  e_flags     : u32; (* Processor-specific flags.          *)
  e_ehsize    : u16; (* ELF header size in bytes.          *)
  e_phentsize : u16; (* Program header table entry size.   *)
  e_phnum     : u16; (* Program header table entry count.  *)
  e_shentsize : u16; (* Section header table entry size.   *)
  e_shnum     : u16; (* Section header table entry count.  *)
  e_shstrndx  : u16; (* Section header string table index. *)
}

let read_header t e_ident =
  assert (t.position = 16);
  ensure t 48 "Header truncated";
  let e_type      = Read.u16 t in
  let e_machine   = Read.u16 t in
  let e_version   = Read.u32 t in
  let e_entry     = Read.u64 t in
  let e_phoff     = Read.u64 t in
  let e_shoff     = Read.u64 t in
  let e_flags     = Read.u32 t in
  let e_ehsize    = Read.u16 t in
  let e_phentsize = Read.u16 t in
  let e_phnum     = Read.u16 t in
  let e_shentsize = Read.u16 t in
  let e_shnum     = Read.u16 t in
  let e_shstrndx  = Read.u16 t in
  { e_type; e_machine; e_version; e_entry;
    e_phoff; e_shoff; e_flags; e_ehsize;
    e_phentsize; e_phnum; e_shentsize;
    e_shnum; e_shstrndx; e_ident }

(* Section header *)
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
  sh_name_str : string;
}

let read_section header t n =
  seek t (header.e_shoff + n * header.e_shentsize);
  ensure t 64 "Shdr truncated";
  let sh_name      = Read.u32 t in
  let sh_type      = Read.u32 t in
  let sh_flags     = Read.u64 t in
  let sh_addr      = Read.u64 t in
  let sh_offset    = Read.u64 t in
  let sh_size      = Read.u64 t in
  let sh_link      = Read.u32 t in
  let sh_info      = Read.u32 t in
  let sh_addralign = Read.u64 t in
  let sh_entsize   = Read.u64 t in
  { sh_name; sh_type; sh_flags; sh_addr;
    sh_offset; sh_size; sh_link; sh_info;
    sh_addralign; sh_entsize; sh_name_str = "" }

let read_section_name shstrndx t shdr =
  let n = shdr.sh_name in
  seek t (shstrndx.sh_offset + n);
  Read.zero_string "Unterminated section name" t
    ~maxlen:(shstrndx.sh_size - n) ()

let read_sections header t =
  let sections = Array.init header.e_shnum (read_section header t) in
  let shstrndx = sections.(header.e_shstrndx) in
  Array.map
    (fun s -> {s with sh_name_str = read_section_name shstrndx t s})
    sections

let read_elf buffer =
  let elf = cursor buffer in
  read_magic elf;
  let e_ident = read_identification elf in
  let header = read_header elf e_ident in
  header, read_sections header elf

let section_body buffer shdr =
  Bigarray.Array1.sub buffer shdr.sh_offset shdr.sh_size

exception Found of section
let find_section sections name =
  try
    Array.iter (fun section ->
        if section.sh_name_str = name then
          raise (Found section))
      sections;
    None
  with Found section ->
    Some section
