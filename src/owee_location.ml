type t = int
let none = 0

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

let extract (f : (_ -> _)) =
  (* Extract code pointer.
     By adding 1 we compensate for the bit we lost because of int-tagging of
     the original address, although the resulting address might be one more
     than the original one. It's ok, we are looking for the beginning of a
     function, and a function is at least one byte long, so being off-by-one is
     ok *)
  Obj.obj (Obj.field (Obj.repr f) 0) * 2 + 1

exception Result of (string * int * int) option

let lookup t =
  if t = none then None
  else
    let lazy buffer = myself in
    let header, sections = Owee_elf.read_elf buffer in
    match Owee_elf.find_section sections ".debug_line" with
    | None -> None
    | Some section ->
      let body = Owee_buf.cursor (Owee_elf.section_body buffer section) in
      let open Owee_debug_line in
      let rec aux () =
        match read_chunk body with
        | None -> None
        | Some (header, chunk) ->
          let check header state address =
            if address <= t && t <= state.address then
              begin match get_filename header state with
                | None -> raise (Result None)
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
