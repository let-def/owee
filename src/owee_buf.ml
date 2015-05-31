let dim = Bigarray.Array1.dim

type t =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type cursor = {
  buffer: t;
  mutable position: int;
}

exception Invalid_format of string
let invalid_format msg = raise (Invalid_format msg)

let assert_format b msg =
  if not b then
    invalid_format msg

let cursor buffer =
  { buffer; position = 0 }

let seek t position =
  t.position <- position

let ensure t count msg =
  if t.position + count > dim t.buffer then
    invalid_format msg

let advance t count = t.position <- t.position + count

let sub t length =
  let result = cursor (Bigarray.Array1.sub t.buffer t.position length) in
  advance t length;
  result

let at_end t = dim t.buffer = t.position

type s8  = int
type u8  = int
type u16 = int
type u32 = int
type u64 = int

type s128 = int
type u128 = int

(* All endian and bit-width dependent code starts here *)
module Read = struct
  let u8 t : u8 =
    let result = t.buffer.{t.position} in
    advance t 1;
    result

  let s8 t : s8 =
    let result = t.buffer.{t.position} in
    advance t 1;
    if result > 0x7F
    then result lor ((-1) lsl 8)
    else result

  let u16 t : u16 =
    let result = t.buffer.{t.position} lor t.buffer.{t.position + 1} lsl 8 in
    advance t 2;
    result

  let u32 t : u32 =
    let result = t.buffer.{t.position}
                 lor t.buffer.{t.position + 1} lsl 8
                 lor t.buffer.{t.position + 2} lsl 16
                 lor t.buffer.{t.position + 3} lsl 24
    in
    advance t 4;
    result

  let u32be = u32

  let u64 t : u64 =
    let result = t.buffer.{t.position}
                 lor t.buffer.{t.position + 1} lsl 8
                 lor t.buffer.{t.position + 2} lsl 16
                 lor t.buffer.{t.position + 3} lsl 24
                 lor t.buffer.{t.position + 4} lsl 32
                 lor t.buffer.{t.position + 5} lsl 40
                 lor t.buffer.{t.position + 6} lsl 48
                 lor t.buffer.{t.position + 7} lsl 56
    in
    advance t 8;
    result

  let uleb128 t : u128 =
    let rec aux t shift acc =
      let x = u8 t in
      let acc = acc lor ((x land 0x7f) lsl shift) in
      if x land 0x80 = 0 then
        acc
      else
        aux t (shift + 7) acc
    in
    aux t 0 0

  let sleb128 t : s128 =
    let rec aux t shift acc =
      let x = u8 t in
      let acc = acc lor ((x land 0x7f) lsl shift) in
      if x land 0x80 = 0 then
        if x land 0x40 = 0
        then acc
        else acc lor -(1 lsl (shift + 7))
      else
        aux t (shift + 7) acc
    in
    aux t 0 0

  let fixed_string t length =
    let {buffer; position} = t in
    let result = Bytes.create length in
    for i = 0 to length - 1 do
      Bytes.set result i (Char.unsafe_chr buffer.{position + i})
    done;
    advance t length;
    Bytes.unsafe_to_string result

  let rec scan_0 msg (b : t) ofs l i =
    if i >= l then
      invalid_format msg
    else if b.{ofs + i} = 0 then
      i
    else
      scan_0 msg b ofs l (i + 1)

  let zero_string msg t ?maxlen () =
    let maxlen = match maxlen with
      | None -> dim t.buffer - t.position
      | Some maxlen -> maxlen
    in
    let length = scan_0 msg t.buffer t.position maxlen 0 in
    let result = fixed_string t length in
    advance t 1;
    result
end