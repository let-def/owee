[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(* `readelf -rW <binary> | sed "s/@[^ ]\+//"` and
   `relocations.exe  <binary> | sed "s/@[^ ]\+//"` should
   return the same thing.
*)

let path, address =
  let argv_len = Array.length Sys.argv in
  if argv_len < 1 || argv_len > 3 then
    (prerr_endline ("Usage: " ^ Sys.argv.(0)
      ^ " my_binary.elf [ADDRESS]"); exit 1)
  else
    let path = Sys.argv.(1) in
    let address =
      if argv_len = 3 then Some (Int64.of_string Sys.argv.(2)) else None
    in
    path, address

let buffer =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let map = Unix.map_file
      fd Bigarray.int8_unsigned Bigarray.c_layout false [|len|]
  in
  Unix.close fd;
  Bigarray.array1_of_genarray map

let _header, sections = Owee_elf.read_elf buffer

let info_to_string = function
  | 7 -> "R_X86_64_JUMP_SLOT"
  | 1 -> "R_X86_64_64"
  | 6 -> "R_X86_64_GLOB_DAT"
  | 5 -> "R_X86_64_COPY"
  | x -> failwith ("Rel type " ^ string_of_int x ^ " unhandled")

let display dynsym strtab relocations name =
  match relocations with
  | None -> ()
  | Some relocations ->
    Printf.printf "Relocation section '%s' at offset %#Lx contains %d entries:\n"
      name
      relocations.Owee_rel.section.sh_offset
      (Array.length relocations.entries);
    Printf.printf "  Offset          Info           Type           Symbol's Value    Symbol's Name + Addend\n";
    Array.iter (fun entry ->
      let sym = Owee_elf.Symbol_table.get dynsym entry in
      let name = Option.value (Owee_elf.Symbol_table.Symbol.name sym strtab) ~default:"<unknown>" in
      Printf.printf "%016Lx  %016Lx %17s %016Lx %s + %Lx\n"
        (Owee_rel_entry.offset entry)
        (Owee_rel_entry.info entry)
        (info_to_string (Owee_rel_entry.type_ entry))
        (Owee_elf.Symbol_table.Symbol.value sym)
        name
        (Option.value ~default:0L (Owee_rel_entry.addend entry))
    ) relocations.entries;
    Printf.printf "\n"

let () =
  match Owee_elf.find_section_body buffer sections ~section_name:".dynsym" with
  | None -> failwith "Symbol table not found"
  | Some dynsym ->
    match Owee_elf.find_dynamic_string_table buffer sections with
    | None -> failwith "String table not found"
    | Some strtab ->
        display dynsym strtab (Owee_rel.read_rel ~type_:`Plt buffer sections) ".rel.plt";
        display dynsym strtab (Owee_rel.read_rel ~type_:`Dyn buffer sections) ".rel.dyn";
        display dynsym strtab (Owee_rel.read_rela ~type_:`Plt buffer sections) ".rela.plt";
        display dynsym strtab (Owee_rel.read_rela ~type_:`Dyn buffer sections) ".rela.dyn"
