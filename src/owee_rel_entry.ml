open! Owee_buf

type t = {
  r_offset : u64;
  r_info : u64;
  r_addend: i64 option;
}

let read_rela cur =
  let r_offset = Read.u64 cur in
  let r_info = Read.u64 cur in
  let r_addend = Some (Read.i64 cur) in
  { r_offset; r_info; r_addend }

let read_rel cur =
  let r_offset = Read.u64 cur in
  let r_info = Read.u64 cur in
  (* Addend is to be extracted from the location to be relocated. *)
  { r_offset; r_info; r_addend = None }

let addend t = t.r_addend
let offset t = t.r_offset
let info t = t.r_info
(* cf https://www.sco.com/developers/gabi/latest/ch4.reloc.html *)
let type_ t = Int64.logand t.r_info 0xffffffffL |> Int64.to_int
let symbol_index t = Int64.shift_right_logical t.r_info 32 |> Int64.to_int
