(* ./decodedline <mybin>
   output should be similar to
   objdump --dwarf=decodedline <mybin>
*)
let path =
  if Array.length Sys.argv <= 1 then
    (prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " my_binary.elf"); exit 1)
  else
    Sys.argv.(1)

let buffer =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let map = Bigarray.Array1.map_file fd
      Bigarray.int8_unsigned Bigarray.c_layout false len in
  Unix.close fd;
  map

let _header, commands = Owee_macho.read buffer

let rec iter_segments f = function
  | [] -> ()
  | (`LC_SEGMENT_64 segment | `LC_SEGMENT_32 segment) :: segments ->
    f segment; iter_segments f segments
  | _ :: segments -> iter_segments f segments

let iter_sections f segment =
  List.iter (f segment) segment.Owee_macho.seg_sections

let check_section segment section =
  Printf.eprintf
    "{
  seg_segname  = %s;
  seg_vmaddr   = %x;
  seg_vmsize   = %x;
  seg_fileoff  = %x;
  seg_filesize = %x;
  seg_maxprot  = .x.;
  seg_initprot = ...;
  seg_flags    = ...;
  seg_sections = ...;
}"
    segment.Owee_macho.seg_segname
    segment.Owee_macho.seg_vmaddr
    segment.Owee_macho.seg_vmsize
    segment.Owee_macho.seg_fileoff
    segment.Owee_macho.seg_filesize;
  Printf.eprintf
    "{
  sec_sectname = %s;
  sec_segname  = %s;
  sec_addr     = %x;
  sec_size     = %x;
  sec_align    = %d;
  sec_relocs   = ...;
  sec_type     = ...;
  sec_user_attrs = ...;
  sec_sys_attrs = ...;
}\n"
    section.Owee_macho.sec_sectname
    section.Owee_macho.sec_segname
    section.Owee_macho.sec_addr
    section.Owee_macho.sec_size
    section.Owee_macho.sec_align

let debug_section segment = function
  | {Owee_macho.sec_sectname = "__debug_line"; sec_segname = "__DWARF"}
    as section ->
    let body = Owee_buf.cursor (
        Bigarray.Array1.sub buffer
          (segment.Owee_macho.seg_fileoff + section.Owee_macho.sec_addr)
          section.Owee_macho.sec_size
      ) in
    let rec aux () =
      match Owee_debug_line.read_chunk body with
      | None -> ()
      | Some (header, chunk) ->
        let check header state () =
          let open Owee_debug_line in
          if not state.end_sequence then
            match get_filename header state with
            | None -> ()
            | Some filename ->
              Printf.printf "%s\t%d\t0x%x\n" filename state.line state.address
        in
        Owee_debug_line.fold_rows (header, chunk) check ();
        aux ()
    in
    aux ()
  | _ -> ()

let () =
  iter_segments (iter_sections check_section) commands;
  iter_segments (iter_sections debug_section) commands
  (*match Owee_elf.find_section sections ".debug_line" with
  | None -> ()
  | Some section ->
    let body = Owee_buf.cursor (Owee_elf.section_body buffer section) in
    let rec aux () =
      match Owee_debug_line.read_chunk body with
      | None -> ()
      | Some (header, chunk) ->
        let check header state () =
          let open Owee_debug_line in
          if not state.end_sequence then
          match get_filename header state with
          | None -> ()
          | Some filename ->
            Printf.printf "%s\t%d\t0x%x\n" filename state.line state.address
        in
        Owee_debug_line.fold_rows (header, chunk) check ();
        aux ()
    in
    aux ()*)
