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

let extract obj =
  (* Extract code pointer.
     By adding 1 we compensate for the bit we lost because of int-tagging of
     the original address, although the resulting address might be one more
     than the original one. It's ok, we are looking for the beginning of a
     function, and a function is at least one byte long, so being off-by-one is
     ok *)
  let cp : t = Obj.obj (Obj.field obj 0) lor 1 in
  (*Printf.printf "cp = 0x%X\n%!" cp;*)
  if cp >= caml_startup__code_end then
    cp
  else
    Obj.obj (Obj.field obj (Obj.size obj - 1)) lor 1
    (*begin
      Printf.printf "object tag:%d size:%d\n%!" (Obj.tag obj) (Obj.size obj);
      for i = 0 to Obj.size obj - 1 do
        let obj = Obj.field obj i in
        if Obj.is_int obj then
          Printf.printf "  - field %i = %d\n" i (Obj.obj obj)
        else if Obj.is_block obj then
          Printf.printf "  - field %i tag:%d size:%d\n%!" i (Obj.tag obj) (Obj.size obj)
        else
          Printf.printf "  - field %i is astract" i
      done;
      extract (Obj.field obj 1)
    end*)
    (*let obj' = Obj.field obj 1 in
    if Obj.tag obj' = Obj.closure_tag then
      extract obj'
    else
      none*)

let extract (f : (_ -> _)) = extract (Obj.repr f)

exception Result of (string * int * int) option

let lookup t =
  if t = none then None
  else
    let lazy buffer = myself in
    let _header, sections = Owee_elf.read_elf buffer in
    match Owee_elf.find_section sections ".debug_line" with
    | None -> None
    | Some section ->
      (*Printf.eprintf "Looking for 0x%X\n" t;*)
      let body = Owee_buf.cursor (Owee_elf.section_body buffer section) in
      let open Owee_debug_line in
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
            if address lsr 1 <= t && t <= state.address lsr 1 then
              begin match get_filename header state with
                | None ->
                  raise (Result None)
                | Some filename ->
                  raise (Result (Some (filename, state.line, state.col)))
              end;
            if state.end_sequence
            then max_int
            else state.address
          in
          let _ : int = interpret header chunk check max_int in
          aux ()
      in
      try aux ()
      with Result r -> r

let locate f = lookup (extract f)
