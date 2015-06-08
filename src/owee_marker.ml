type 'result service = ..

type _ service +=
   | Name : string service
   | Traverse : ((Obj.t -> 'acc -> 'acc) -> 'acc -> 'acc) service

type 'a service_result =
  | Success of 'a
  | Unsupported_service
  | Unmanaged_object

let magic_potion = Obj.repr (ref ())

type 'a marker = {
  magic_potion: Obj.t;
  service: 'result. 'a -> 'result service -> 'result service_result;
}

let is_marker obj =
  Obj.tag obj = 0 && Obj.size obj = 2 && Obj.field obj 0 == magic_potion

exception Found of Obj.t marker

let query_service t service =
  let obj = Obj.repr t in
  try
    if Obj.tag obj < Obj.lazy_tag then
      for i = 0 to Obj.size obj - 1 do
        let obj' = (Obj.field obj i) in
        if is_marker obj' then
          raise (Found (Obj.obj obj'))
      done;
    Unmanaged_object
  with Found marker ->
    marker.service obj service

module type T0 = sig
  type t
  val service : t -> 'result service -> 'result service_result
end

module Unsafe0 (M : T0) : sig
  val marker : M.t marker
end = struct
  let marker = M.({ magic_potion; service })
end

type 'a marked = {
  cell: 'a;
  marker: 'a marked marker;
}
let get t = t.cell

module Safe0 (M : T0) : sig
  val mark : M.t -> M.t marked
end = struct
  include Unsafe0(struct
      type t = M.t marked
      let service obj request = M.service obj.cell request
    end)
  let mark cell = {cell; marker}
end

(******)

module type T1 = sig
  type 'x t
  val service : 'x t -> 'result service -> 'result service_result
end

module Unsafe1 (M : T1) : sig
  val marker : 'x M.t marker
end = struct
  let marker = M.({ magic_potion; service })
end

module Safe1 (M : T1) : sig
  val mark : 'a M.t -> 'a M.t marked
end = struct
  include Unsafe1(struct
      type 'a t = 'a M.t marked
      let service obj request = M.service obj.cell request
    end)
  let mark cell = {cell; marker}
end

module type T2 = sig
  type ('x, 'y) t
  val service : ('x, 'y) t -> 'result service -> 'result service_result
end

module Unsafe2 (M : T2) : sig
  val marker : ('x, 'y) M.t marker
end = struct
  let marker = M.({ magic_potion; service })
end

module Safe2 (M : T2) : sig
  val mark : ('a, 'b) M.t -> ('a, 'b) M.t marked
end = struct
  include Unsafe2(struct
      type ('a, 'b) t = ('a, 'b) M.t marked
      let service obj request = M.service obj.cell request
    end)
  let mark cell = {cell; marker}
end

module type T3 = sig
  type ('x, 'y, 'z) t
  val service : ('x, 'y, 'z) t -> 'result service -> 'result service_result
end

module Unsafe3 (M : T3) : sig
  val marker : ('x, 'y, 'z) M.t marker
end = struct
  let marker = M.({ magic_potion; service })
end

module Safe3 (M : T3) : sig
  val mark : ('a, 'b, 'c) M.t -> ('a, 'b, 'c) M.t marked
end = struct
  include Unsafe3(struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.t marked
      let service obj request = M.service obj.cell request
    end)
  let mark cell = {cell; marker}
end
