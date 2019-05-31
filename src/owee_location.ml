type t

external extract : (_ -> _) -> t =
  "ml_owee_code_pointer" "ml_owee_code_pointer" "noalloc"

let myself = lazy begin
  (* 64-bit linux only :p *)
  let pid = Unix.getpid () in
  let path = "/proc/" ^ string_of_int pid ^ "/exe" in
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let map =
    Bigarray.array1_of_genarray
      (Unix.map_file fd Bigarray.int8_unsigned
         Bigarray.c_layout false [|len|]) in
  Unix.close fd;
  map
end

let force_int i : t = Obj.magic (lnot i lxor -1)
let none = force_int 0

let count_rows body =
  let open Owee_debug_line in
  let cursor = Owee_buf.cursor body in
  let count = ref 0 in
  let rec aux () =
    match read_chunk cursor with
    | None -> !count
    | Some line_program ->
      let check _header state address =
        if address <> max_int then
          incr count;
        if state.end_sequence
        then max_int
        else state.address
      in
      let _ : int = fold_rows line_program check max_int in
      aux ()
  in
  aux ()

let store_rows body array =
  let open Owee_debug_line in
  let cursor = Owee_buf.cursor body in
  let index = ref 0 in
  let prev_line = ref 0 in
  let prev_col  = ref 0 in
  let prev_file = ref None in
  let rec aux () =
    match read_chunk cursor with
    | None -> ()
    | Some (header, chunk) ->
      let check _header state address =
        if address <> max_int then
          begin
            array.(!index) <-
              address,
              state.address,
              (match !prev_file with
               | Some fname -> Some (fname, !prev_line, !prev_col)
               | None -> None);
            incr index
          end;
        prev_file := get_filename header state;
        prev_line := state.line;
        prev_col := state.col;
        if state.end_sequence then
          max_int
        else
          state.address
      in
      let _ : int = fold_rows (header, chunk) check max_int in
      aux ()
  in
  aux ()

let cache = lazy begin
  let lazy buffer = myself in
  let _header, sections = Owee_elf.read_elf buffer in
  match Owee_elf.find_section sections ".debug_line" with
  | None -> [||]
  | Some section ->
    (*Printf.eprintf "Looking for 0x%X\n" t;*)
    let body = Owee_elf.section_body buffer section in
    let count = count_rows body in
    let cache = Array.make count (max_int, max_int, None) in
    store_rows body cache;
    cache
end

let rec bsearch cache i j address =
  if i >= j then None
  else
    let k = (i + j) / 2 in
    let a0, a1, result = cache.(k) in
    if a0 lsr 1 <= address && address < a1 lsr 1 then
      result
    else if address < a0 lsr 1 then
      bsearch cache i k address
    else
      bsearch cache (k + 1) j address

let lookup t =
  if t = none then None
  else if Obj.is_int (Obj.repr t) then
    let lazy cache = cache in
    bsearch cache 0 (Array.length cache) (Obj.magic t : int)
  else
    let t = Obj.repr t in
    assert (Obj.tag t = 0);
    assert (Obj.size t = 1);
    assert (Obj.size (Obj.field t 0) = 3);
    Obj.obj t

let locate f = lookup (extract f)

external nearest_symbol : t -> string = "ml_owee_code_pointer_symbol"

let demangled_symbol s =
  let len = String.length s in
  if len <= 4
     || s.[0] <> 'c'
     || s.[1] <> 'a'
     || s.[2] <> 'm'
     || s.[3] <> 'l'
  then s
  else
    let end_of_name = ref len in
    let skip_at_end = function
      | '0'..'9' -> true
      | '_' -> true
      | _ -> false
    in
    while !end_of_name > 4 && skip_at_end s.[!end_of_name - 1] do
      decr end_of_name
    done;
    let buf = Buffer.create len in
    let skip = ref false in
    for i = 4 to !end_of_name - 1 do
      if !skip then
        skip := false
      else if s.[i] = '_'
         && i + 1 < len
         && s.[i + 1] = '_'
      then (Buffer.add_char buf '.'; skip := true)
      else
        Buffer.add_char buf s.[i]
    done;
    if !end_of_name < len then
      (Buffer.add_char buf '/';
       let e = !end_of_name + 1 in
       Buffer.add_substring buf s e (len - e));
    Buffer.contents buf

let nearest_demangled_symbol t =
  demangled_symbol (nearest_symbol t)
