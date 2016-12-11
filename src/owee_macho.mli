type magic = MAGIC32 | MAGIC64 | CIGAM32 | CIGAM64

type cpu_type =
    [ `ARM | `POWERPC | `POWERPC64 | `Unknown of int | `X86 | `X86_64 ]

type cpu_subtype =
    [ `ARM_ALL
    | `ARM_V4T
    | `ARM_V6
    | `CELERON
    | `CELERON_MOBILE
    | `I386
    | `I386_ALL
    | `I486
    | `I486SX
    | `INTEL_FAMILY
    | `INTEL_FAMILY_MAX
    | `INTEL_MODEL
    | `INTEL_MODEL_ALL
    | `ITANIUM
    | `ITANIUM_2
    | `Intel
    | `PENT
    | `PENTII_M3
    | `PENTII_M5
    | `PENTIUM_3
    | `PENTIUM_3_M
    | `PENTIUM_3_XEON
    | `PENTIUM_4
    | `PENTIUM_4_M
    | `PENTIUM_M
    | `PENTPRO
    | `POWERPC_601
    | `POWERPC_602
    | `POWERPC_603
    | `POWERPC_603e
    | `POWERPC_603ev
    | `POWERPC_604
    | `POWERPC_604e
    | `POWERPC_620
    | `POWERPC_7400
    | `POWERPC_7450
    | `POWERPC_750
    | `POWERPC_970
    | `POWERPC_ALL
    | `Unknown of int
    | `X86_64_ALL
    | `X86_ALL
    | `X86_ARCH1
    | `XEON
    | `XEON_MP ]
type file_type =
    [ `BUNDLE
    | `CORE
    | `DSYM
    | `DYLIB
    | `DYLIB_STUB
    | `DYLINKER
    | `EXECUTE
    | `OBJECT
    | `PRELOAD
    | `Unknown of int ]
type header_flag =
    [ `ALLMODSBOUND
    | `ALLOW_STACK_EXECUTION
    | `BINDATLOAD
    | `BINDS_TO_WEAK
    | `CANONICAL
    | `DYLDLINK
    | `FORCE_FLAT
    | `INCRLINK
    | `NOFIXPREBINDING
    | `NOMULTIDEFS
    | `NOUNDEFS
    | `NO_REEXPORTED_DYLIBS
    | `PIE
    | `PREBINDABLE
    | `PREBOUND
    | `ROOT_SAFE
    | `SETUID_SAFE
    | `SPLIT_SEGS
    | `SUBSECTIONS_VIA_SYMBOLS
    | `TWOLEVEL
    | `WEAK_DEFINES ]
type header = {
  magic : magic;
  cpu_type : cpu_type;
  cpu_subtype : cpu_subtype;
  file_type : file_type;
  flags : header_flag list;
}
type r_type =
    [ `GENERIC_RELOC_LOCAL_SECTDIFF
    | `GENERIC_RELOC_PAIR
    | `GENERIC_RELOC_PB_LA_PTR
    | `GENERIC_RELOC_SECTDIFF
    | `GENERIC_RELOC_VANILLA
    | `PPC_RELOC_BR14
    | `PPC_RELOC_BR24
    | `PPC_RELOC_HA16
    | `PPC_RELOC_HA16_SECTDIFF
    | `PPC_RELOC_HI16
    | `PPC_RELOC_HI16_SECTDIFF
    | `PPC_RELOC_JBSR
    | `PPC_RELOC_LO14
    | `PPC_RELOC_LO14_SECTDIFF
    | `PPC_RELOC_LO16
    | `PPC_RELOC_LO16_SECTDIFF
    | `PPC_RELOC_LOCAL_SECTDIFF
    | `PPC_RELOC_PAIR
    | `PPC_RELOC_PB_LA_PTR
    | `PPC_RELOC_SECTDIFF
    | `PPC_RELOC_VANILLA
    | `Unknown of int
    | `X86_64_RELOC_BRANCH
    | `X86_64_RELOC_GOT
    | `X86_64_RELOC_GOT_LOAD
    | `X86_64_RELOC_SIGNED
    | `X86_64_RELOC_SIGNED_1
    | `X86_64_RELOC_SIGNED_2
    | `X86_64_RELOC_SIGNED_4
    | `X86_64_RELOC_SUBTRACTOR
    | `X86_64_RELOC_UNSIGNED ]

type relocation_info = {
  ri_address : int;
  ri_symbolnum : Owee_buf.u32;
  ri_pcrel : bool;
  ri_length : Owee_buf.u32;
  ri_extern : bool;
  ri_type : r_type;
}

type scattered_relocation_info = {
  rs_pcrel : bool;
  rs_length : Owee_buf.u32;
  rs_type : r_type;
  rs_address : Owee_buf.u32;
  rs_value : Owee_buf.s32;
}

val read_scattered_relocation_info :
  'a -> header -> int -> Owee_buf.s32 -> scattered_relocation_info

type relocation =
    [ `Relocation_info of relocation_info
    | `Scattered_relocation_info of scattered_relocation_info ]

type sec_type =
    [ `S_16BYTE_LITERALS
    | `S_4BYTE_LITERALS
    | `S_8BYTE_LITERALS
    | `S_COALESCED
    | `S_CSTRING_LITERALS
    | `S_DTRACE_DOF
    | `S_GB_ZEROFILL
    | `S_INTERPOSING
    | `S_LAZY_DYLIB_SYMBOL_POINTERS
    | `S_LAZY_SYMBOL_POINTERS
    | `S_LITERAL_POINTERS
    | `S_MOD_INIT_FUNC_POINTERS
    | `S_MOD_TERM_FUNC_POINTERS
    | `S_NON_LAZY_SYMBOL_POINTERS
    | `S_REGULAR
    | `S_SYMBOL_STUBS
    | `S_ZEROFILL
    | `Unknown of int ]

type sec_user_attr =
    [ `DEBUG
    | `LIVE_SUPPORT
    | `NO_DEAD_STRIP
    | `NO_TOC
    | `PURE_INSTRUCTIONS
    | `SELF_MODIFYING_CODE
    | `STRIP_STATIC_SYMS ]

type sec_sys_attr = [ `EXT_RELOC | `LOC_RELOC | `SOME_INSTRUCTIONS ]

type section = {
  sec_sectname : string;
  sec_segname : string;
  sec_addr : Owee_buf.u64;
  sec_size : Owee_buf.u64;
  sec_align : int;
  sec_relocs : relocation list;
  sec_type : sec_type;
  sec_user_attrs : sec_user_attr list;
  sec_sys_attrs : sec_sys_attr list;
}

type vm_prot = [ `EXECUTE | `READ | `WRITE ]

type seg_flag = [ `HIGHVM | `NORELOC ]

type segment = {
  seg_segname : string;
  seg_vmaddr : Owee_buf.u64;
  seg_vmsize : Owee_buf.u64;
  seg_fileoff : Owee_buf.u64;
  seg_filesize : Owee_buf.u64;
  seg_maxprot : vm_prot list;
  seg_initprot : vm_prot list;
  seg_flags : seg_flag list;
  seg_sections : section list;
}

type sym_type =
    [ `ABS
    | `BCOMM
    | `BINCL
    | `BNSYM
    | `ECOML
    | `ECOMM
    | `EINCL
    | `ENSYM
    | `ENTRY
    | `EXCL
    | `FNAME
    | `FUN
    | `GSYM
    | `INDR
    | `LBRAC
    | `LCSYM
    | `LENG
    | `LSYM
    | `OLEVEL
    | `OPT
    | `OSO
    | `PARAMS
    | `PBUD
    | `PC
    | `PSYM
    | `RBRAC
    | `RSYM
    | `SECT
    | `SLINE
    | `SO
    | `SOL
    | `SSYM
    | `STSYM
    | `UNDF
    | `Unknown of int
    | `VERSION ]

type reference_flag =
    [ `DEFINED
    | `LIBRARY_ORDINAL of Owee_buf.u16
    | `PRIVATE_DEFINED
    | `PRIVATE_UNDEFINED_LAZY
    | `PRIVATE_UNDEFINED_NON_LAZY
    | `REFERENCED_DYNAMICALLY
    | `SYM_WEAK_DEF
    | `SYM_WEAK_REF
    | `UNDEFINED_LAZY
    | `UNDEFINED_NON_LAZY
    | `Unknown of int ]

type symbol = {
  sym_name : string;
  sym_type : sym_type;
  sym_pext : bool;
  sym_ext : bool;
  sym_sect : Owee_buf.u8;
  sym_flags :
    [ `Flags of reference_flag list | `Uninterpreted of Owee_buf.u16 ];
  sym_value : Owee_buf.u64;
}

type dylib_module = {
  dylib_module_name_offset : Owee_buf.u32;
  dylib_ext_def_sym : Owee_buf.u32 * Owee_buf.u32;
  dylib_ref_sym : Owee_buf.u32 * Owee_buf.u32;
  dylib_local_sym : Owee_buf.u32 * Owee_buf.u32;
  dylib_ext_rel : Owee_buf.u32 * Owee_buf.u32;
  dylib_init : Owee_buf.u32 * Owee_buf.u32;
  dylib_term : Owee_buf.u32 * Owee_buf.u32;
  dylib_objc_module_info_addr : Owee_buf.u32;
  dylib_objc_module_info_size : Owee_buf.u64;
}
type toc_entry = {
  symbol_index : Owee_buf.u32;
  module_index : Owee_buf.u32;
}

type dynamic_symbol_table = {
  localSyms : Owee_buf.u32 * Owee_buf.u32;
  extDefSyms : Owee_buf.u32 * Owee_buf.u32;
  undefSyms : Owee_buf.u32 * Owee_buf.u32;
  toc_entries : toc_entry list;
  modules : dylib_module list;
  extRefSyms : Owee_buf.u32 list;
  indirectSyms : Owee_buf.u32 list;
  extRels : relocation list;
  locRels : relocation list;
}
type dylib = {
  dylib_name : string;
  dylib_timestamp : Owee_buf.u32;
  dylib_current_version : Owee_buf.u32;
  dylib_compatibility_version : Owee_buf.u32;
}

type command =
    [ `LC_CODE_SIGNATURE of Owee_buf.u32 * Owee_buf.u32
    | `LC_DYSYMTAB of dynamic_symbol_table
    | `LC_ID_DYLIB of dylib
    | `LC_ID_DYLINKER of string
    | `LC_LOAD_DYLIB of dylib
    | `LC_LOAD_DYLINKER of string
    | `LC_LOAD_WEAK_DYLIB of dylib
    | `LC_PREBIND_CKSUM of Owee_buf.u32
    | `LC_PREBOUND_DYLIB of string * Owee_buf.u8 list
    | `LC_ROUTINES_32 of Owee_buf.u32 * Owee_buf.u32
    | `LC_ROUTINES_64 of Owee_buf.u64 * Owee_buf.u64
    | `LC_RPATH of string
    | `LC_SEGMENT_32 of segment
    | `LC_SEGMENT_64 of segment
    | `LC_SEGMENT_SPLIT_INFO of Owee_buf.u32 * Owee_buf.u32
    | `LC_SUB_CLIENT of string
    | `LC_SUB_FRAMEWORK of string
    | `LC_SUB_LIBRARY of string
    | `LC_SUB_UMBRELLA of string
    | `LC_SYMTAB of symbol array * Owee_buf.t
    | `LC_THREAD of (Owee_buf.u32 * Owee_buf.u32 list) list
    | `LC_TWOLEVEL_HINTS of (Owee_buf.u32 * Owee_buf.u32) list
    | `LC_UNHANDLED of int * Owee_buf.t
    | `LC_UNIXTHREAD of (Owee_buf.u32 * Owee_buf.u32 list) list
    | `LC_UUID of string ]

val read : Owee_buf.t -> header * command list

val section_body : Owee_buf.t -> section -> Owee_buf.t
