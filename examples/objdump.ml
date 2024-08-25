(**
    OCaml implementation of objdump for MachO binaries.
    should be equivalent to

    objdump --section-headers <binaries>
*)
let cpu_type_to_string = function
  | `X86 -> "x86_32"
  | `X86_64 -> "x86_64"
  | `ARM -> "arm32"
  | `ARM64 -> "arm64"
  | `POWERPC -> "ppc32"
  | `POWERPC64 -> "ppc64"
  | _unknown -> "unknown"

let on_segment f = function
  | Owee_macho.LC_SEGMENT_64 segment | Owee_macho.LC_SEGMENT_32 segment ->
    let lazy segment = segment in
    f segment
  | _ -> ()

let iter_sections f segment =
  Array.iter (f segment) segment.Owee_macho.seg_sections

let debug_section index (_segment : Owee_macho.segment) (section : Owee_macho.section) =
  Printf.printf "%3i %-16s %08Lx %016Lx %-10s\n" index
    section.sec_sectname
    section.sec_size
    section.sec_addr
    section.sec_segname

let print_summary file (header : Owee_macho.header) =
  Printf.printf "\n%s:\tfile format mach-o %s\n\n" file (cpu_type_to_string header.cpu_type)

let print_section_headers file =
  let open Printf in
  let buffer = Owee_buf.map_binary file in
  let header, commands = Owee_macho.read buffer in

  print_summary file header;
  printf "Sections: \n";
  printf "%3s %-16s %-8s %-16s %-10s\n" "Idx" "Name" "Size" "VMA" "Type";
  List.iteri (fun index -> on_segment (iter_sections (debug_section index))) commands

let run section_headers files () =
  match section_headers with
  | true -> List.iter print_section_headers files
  | false ->
     Printf.printf "Section headers flag required please call with `--section-headers`.\n";
     exit(2)

open Cmdliner

let section_headers =
  let doc = "Display summaries of the headers for each section.." in
  let info = Arg.(info ["section-headers"] ~doc) in
  Arg.value (Arg.flag info)

let files =
  let doc = "Object FILEs to read." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE" ~doc)

let section_headers_t =
  Term.(const run $ section_headers $ files $ const ())

let cmd =
  let doc = "OCaml object file dumper." in
  let info = Cmd.info "objdump" ~doc in
  Cmd.v info section_headers_t

let () = exit @@ Cmd.eval ~catch:true cmd