open Owee_buf
open Owee_elf

type t = {
  section : Owee_elf.section;
  entries : Owee_rel_entry.t array;
}

let read ~type_ ~section_name ~read buf sections =
  let section_name = match type_ with
    | `Plt -> section_name ^ ".plt"
    | `Dyn -> section_name ^ ".dyn"
  in
  let section = Owee_elf.find_section sections section_name in
  let section_body = Owee_elf.find_section_body buf sections ~section_name in
  match section, section_body with
  | Some header, Some body ->
    let nr_rel_entries = Int64.div (header : section).sh_size  header.sh_entsize in
    let cur = cursor body in
    let entries =
      Array.init (Int64.to_int nr_rel_entries) (fun _ -> read cur)
    in
    Some { section = header;
           entries }
  | _ -> None

let read_rela = read ~section_name:".rela" ~read:Owee_rel_entry.read_rela
let read_rel = read ~section_name:".rel" ~read:Owee_rel_entry.read_rel
