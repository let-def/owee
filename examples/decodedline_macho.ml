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

let on_segment f = function
  | Owee_macho.LC_SEGMENT_64 segment | Owee_macho.LC_SEGMENT_32 segment ->
    let lazy segment = segment in
    f segment
  | _ -> ()

let iter_sections f segment =
  Array.iter (f segment) segment.Owee_macho.seg_sections

let debug_section segment = function
  | {Owee_macho.sec_sectname = "__debug_line"; sec_segname = "__DWARF"}
    as section ->
    let body = Owee_buf.cursor (Owee_macho.section_body buffer segment section) in
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

let () = List.iter (on_segment (iter_sections debug_section)) commands
