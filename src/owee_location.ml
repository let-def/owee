type t = int
let none = 0

external owee_get_symbol : string -> t =
  "ml_owee_get_symbol" "ml_owee_get_symbol" "noalloc"

(*let caml_startup_code_begin =
  owee_get_symbol "caml_startup_code_begin"*)
let caml_startup__code_end =
  owee_get_symbol "caml_startup__code_end"

(*let () =
  Printf.printf "caml_startup__code_end = 0x%X\n%!"
    caml_startup__code_end*)

let myself = lazy begin
  (* 64-bit linux only :p *)
  let pid = Unix.getpid () in
  let path = "/proc/" ^ string_of_int pid ^ "/exe" in
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let map = Bigarray.Array1.map_file fd
      Bigarray.int8_unsigned Bigarray.c_layout false len in
  Unix.close fd;
  map
end

let force_int i = (lnot i lxor -1)

let extract obj =
  (* Extract code pointer.
     By adding 1 we compensate for the bit we lost because of int-tagging of
     the original address, although the resulting address might be one more
     than the original one. It's ok, we are looking for the beginning of a
     function, and a function is at least one byte long, so being off-by-one is
     ok *)
  let cp : t = force_int (Obj.obj (Obj.field obj 0))  in
  if cp >= caml_startup__code_end
  then cp
  else force_int (Obj.obj (Obj.field obj (Obj.size obj - 1)))

let extract (f : (_ -> _)) = extract (Obj.repr f)

exception Result of (string * int * int) option

let lookup t =
  if t = none then None
  else if Obj.is_int (Obj.repr t) then
    let lazy buffer = myself in
    let _header, sections = Owee_elf.read_elf buffer in
    match Owee_elf.find_section sections ".debug_line" with
    | None -> None
    | Some section ->
      (*Printf.eprintf "Looking for 0x%X\n" t;*)
      let body = Owee_buf.cursor (Owee_elf.section_body buffer section) in
      let open Owee_debug_line in
      let prev_line = ref 0 in
      let prev_col  = ref 0 in
      let prev_file = ref None in
      let rec aux () =
        match read_chunk body with
        | None -> None
        | Some (header, chunk) ->
          let check header state address =
            (*Printf.eprintf "%s:%d 0x%X-0x%X\n"
              (match get_filename header state with
               | None -> "?"
               | Some filename -> filename
              ) state.line address state.address;*)
            if address lsr 1 <= t && t < state.address lsr 1 then
              begin match !prev_file with
                | None ->
                  raise (Result None)
                | Some filename ->
                  raise (Result (Some (filename, !prev_line, !prev_col)))
              end;
            prev_file := get_filename header state;
            prev_line := state.line;
            prev_col := state.col;
            if state.end_sequence
            then max_int
            else state.address
          in
          let _ : int = interpret header chunk check max_int in
          aux ()
      in
      try aux ()
      with Result r -> r
  else
    let t = Obj.repr t in
    assert (Obj.tag t = 0);
    assert (Obj.size t = 1);
    assert (Obj.size (Obj.field t 0) = 3);
    Obj.obj t


let locate f = lookup (extract f)
