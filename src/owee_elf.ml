[@@@ocaml.warning "+a-4-9-30-40-41-42"]

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

let elfclass64 = 2

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
  then begin
    invalid_format "Incorrect padding after identification"
  end;
  if elf_class != elfclass64 then begin
    failwith "owee only supports ELFCLASS64 files"
  end;
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

let find_section_body buf sections ~section_name =
  match find_section sections section_name with
  | None -> None
  | Some section -> Some (section_body buf section)

module String_table = struct
  type t = Owee_buf.t

  let get_string t ~index =
    if index < 0 || index >= Owee_buf.dim t then
      None
    else
      let cursor = Owee_buf.cursor t ~at:index in
      Some (Owee_buf.Read.zero_string "boo!" cursor ())
end

let find_string_table buf sections =
  find_section_body buf sections ~section_name:".strtab"

module Symbol_table = struct
  type t = Owee_buf.t

  module Symbol = struct
    type t = {
      st_name : u32;
      st_info : u8;
      st_other : u8;
      st_shndx : u16;
      st_value : u64;
      st_size : u64;
    }

    let struct_size = (32 + 8 + 8 + 16 + 64 + 64) / 8

    type type_attribute =
      | Notype
      | Object
      | Func
      | Section
      | File
      | Common
      | TLS
      | GNU_ifunc
      | Other of int

    type binding_attribute =
      | Local
      | Global
      | Weak
      | GNU_unique
      | Other of int

    type visibility =
      | Default
      | Internal
      | Hidden
      | Protected

    let name t string_table =
      String_table.get_string string_table ~index:t.st_name

    let value t = Int64.of_int t.st_value
    let size_in_bytes t = Int64.of_int t.st_size

    let type_attribute t =
      match t.st_info land 0xf with
      | 0 -> Notype
      | 1 -> Object
      | 2 -> Func
      | 3 -> Section
      | 4 -> File
      | 5 -> Common
      | 6 -> TLS
      | 10 -> GNU_ifunc
      | x -> Other x

    let binding_attribute t =
      match t.st_info lsr 4 with
      | 0 -> Local
      | 1 -> Global
      | 2 -> Weak
      | 10 -> GNU_unique
      | x -> Other x

    let visibility t =
      match t.st_other land 0x3 with
      | 0 -> Default
      | 1 -> Internal
      | 2 -> Hidden
      | 3 -> Protected
      | _ -> assert false

    let section_header_table_index t = t.st_shndx
  end

  (* CR-someday mshinwell: [int] may not strictly be correct *)
  let num_symbols t =
    (Owee_buf.dim t) / Symbol.struct_size

  let get_symbol t ~index =
    if index < 0 || index >= num_symbols t then begin
      None
    end else begin
      let cursor = Owee_buf.cursor t ~at:(index * Symbol.struct_size) in
      let st_name = Owee_buf.Read.u32 cursor in
      let st_info = Owee_buf.Read.u8 cursor in
      let st_other = Owee_buf.Read.u8 cursor in
      let st_shndx = Owee_buf.Read.u16 cursor in
      let st_value = Owee_buf.Read.u64 cursor in
      let st_size = Owee_buf.Read.u64 cursor in
      let symbol : Symbol.t =
        { st_name; st_info; st_other; st_shndx; st_value; st_size; }
      in
      Some symbol
    end

  let get_symbol_exn t ~index =
    match get_symbol t ~index with
    | Some symbol -> symbol
    | None -> failwith "Owee_elf.get_symbol_exn: index out of bounds"

  let iter t ~f =
    for index = 0 to (num_symbols t) - 1 do
      f (get_symbol_exn t ~index)
    done

  let fold t ~init ~f =
    let acc = ref init in
    for index = 0 to (num_symbols t) - 1 do
      acc := f (get_symbol_exn t ~index) !acc
    done;
    !acc

  let symbols_enclosing_address t ~address =
    fold t ~init:[] ~f:(fun sym acc ->
      let sym_start = Symbol.value sym in
      let sym_end =
        Int64.add (Symbol.value sym) (Symbol.size_in_bytes sym)
      in
      if Int64.compare address sym_start >= 0
        && Int64.compare address sym_end < 0
      then
        sym::acc
      else
        acc)

  let functions_enclosing_address t ~address =
    List.filter (fun sym ->
        match Symbol.type_attribute sym with
        | Func -> true
        | Notype | Object | Section | File
        | Common | TLS | GNU_ifunc | Other _ -> false)
      (symbols_enclosing_address t ~address)
end

let find_symbol_table buf sections =
  find_section_body buf sections ~section_name:".symtab"
