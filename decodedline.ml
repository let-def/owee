(* ./decodedline <mybin>
   output should be similar to
   objdump --dwarf=decodedline <mybin>
*)
let path = Sys.argv.(1)

let buffer =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let map = Bigarray.Array1.map_file fd
      Bigarray.int8_unsigned Bigarray.c_layout false len in
  Unix.close fd;
  map

let _header, sections = Owee_elf.read_elf buffer

let () =
  match Owee_elf.find_section sections ".debug_line" with
  | None -> ()
  | Some section ->
    let body = Owee_buf.cursor (Owee_elf.section_body buffer section) in
    let rec aux () =
      match Owee_debug_line.read_chunk body with
      | None -> ()
      | Some (header, chunk) ->
        let check header state () =
          if not state.Owee_debug_line.end_sequence then
          match Owee_debug_line.get_filename header state with
          | None -> ()
          | Some filename ->
            Printf.printf "%s\t%d\t0x%x\n" filename state.line state.address
        in
        Owee_debug_line.interpret header chunk check ();
        aux ()
    in
    aux ()
