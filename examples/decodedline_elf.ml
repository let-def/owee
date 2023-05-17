(* ./decodedline <mybin>
   output should be similar to
   objdump --dwarf=decodedline <mybin>
*)
let usage () =
  prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " my_binary.elf [-no-address]");
  exit 1

let path =
  if Array.length Sys.argv <= 1 then
    usage ()
  else
    Sys.argv.(1)

let print_address =
  if Array.length Sys.argv <= 2 then
    true
  else match Sys.argv.(2) with
    | "-no-address" -> false
    | _ -> usage ()

let buffer = Owee_buf.map_binary path

let _header, sections = Owee_elf.read_elf buffer

let () =
  match Owee_elf.find_section sections ".debug_line" with
  | None -> ()
  | Some section ->
    let pointers_to_other_sections =
      Owee_elf.debug_line_pointers buffer sections
    and body =
      Owee_buf.cursor (Owee_elf.section_body buffer section)
    in
    let rec aux () =
      match Owee_debug_line.read_chunk ~pointers_to_other_sections body with
      | None -> ()
      | Some (header, chunk) ->
        let check header state () =
          let open Owee_debug_line in
          if not state.end_sequence then
          match get_filename header state with
          | None -> ()
          | Some filename ->
            Printf.printf "%s\t%d" filename state.line;
            if print_address then
              Printf.printf "\t0x%x\n" state.address
            else
              print_newline ()
        in
        Owee_debug_line.fold_rows (header, chunk) check ();
        aux ()
    in
    aux ()
