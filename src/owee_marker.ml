type 'result service = ..

type _ service +=
   | Name : string service
   | Traverse : ((Obj.t -> 'acc -> 'acc) -> 'acc -> 'acc) service

exception Unmanaged_object
exception Unsupported_service

type magic_potion
let magic_potion = Obj.repr (ref ())

type 'a marker = {
  magic_potion: Obj.t;
  service: 'result. 'a -> 'result service -> 'result;
}
exception Found of Obj.t marker

let is_marker obj =
  Obj.tag obj = 0 && Obj.size obj = 2 && Obj.field obj 0 == magic_potion

let query_service t service =
  let obj = Obj.repr t in
  try
    if Obj.tag obj < Obj.lazy_tag then
      for i = 0 to Obj.size obj - 1 do
        let obj' = (Obj.field obj i) in
        if is_marker obj' then
          raise (Found (Obj.obj obj'))
      done;
    raise Unmanaged_object
  with Found marker ->
    marker.service obj service

module type T0 = sig
  type t
  val service : t -> 'result service -> 'result
end

module Unsafe0 (M : T0) : sig
  val marker : M.t marker
end = struct
  let marker = M.({ magic_potion; service })
end

module Safe0 (M : T0) : sig
  type t
  val inj : M.t -> t
  val prj : t -> M.t
end = struct
  type wrapper = {
    cell: M.t;
    marker: wrapper marker;
  }
  type t = wrapper

  include Unsafe0(struct
      type t = wrapper
      let service obj request = M.service obj.cell request
    end)

  let inj cell = {cell; marker}
  let prj t = t.cell
end

(******)

module type T1 = sig
  type 'x t
  val service : 'x t -> 'result service -> 'a
end

module type T2 = sig
  type ('x, 'y) t
  val service : ('x, 'y) t -> 'result service -> 'a
end

module type T3 = sig
  type ('x, 'y, 'z) t
  val service : ('x, 'y, 'z) t -> 'result service -> 'a
end

module Unsafe1 (M : T1) : sig
  val marker : 'x M.t marker
end = struct
  let marker = M.({ magic_potion; service })
end

module Unsafe2 (M : T2) : sig
  val marker : ('x, 'y) M.t marker
end = struct
  let marker = M.({ magic_potion; service })
end

module Unsafe3 (M : T3) : sig
  val marker : ('x, 'y, 'z) M.t marker
end = struct
  let marker = M.({ magic_potion; service })
end

module Safe1 (M : T1) : sig
  type 'a t
  val inj : 'a M.t -> 'a t
  val prj : 'a t -> 'a M.t
end = struct
  type 'a wrapper = {
    cell: 'a M.t;
    marker: 'a wrapper marker;
  }
  type 'a t = 'a wrapper

  include Unsafe1(struct
      type 'a t = 'a wrapper
      let service obj request = M.service obj.cell request
    end)

  let inj cell = {cell; marker}
  let prj t = t.cell
end

module Safe2 (M : T2) : sig
  type ('a, 'b) t
  val inj : ('a, 'b) M.t -> ('a, 'b) t
  val prj : ('a, 'b) t -> ('a, 'b) M.t
end = struct
  type ('a, 'b) wrapper = {
    cell: ('a, 'b) M.t;
    marker: ('a, 'b) wrapper marker;
  }
  type ('a, 'b) t = ('a, 'b) wrapper

  include Unsafe2(struct
      type ('a, 'b) t = ('a, 'b) wrapper
      let service obj request = M.service obj.cell request
    end)

  let inj cell = {cell; marker}
  let prj t = t.cell
end

module Safe3 (M : T3) : sig
  type ('a, 'b, 'c) t
  val inj : ('a, 'b, 'c) M.t -> ('a, 'b, 'c) t
  val prj : ('a, 'b, 'c) t -> ('a, 'b, 'c) M.t
end = struct
  type ('a, 'b, 'c) wrapper = {
    cell: ('a, 'b, 'c) M.t;
    marker: ('a, 'b, 'c) wrapper marker;
  }
  type ('a, 'b, 'c) t = ('a, 'b, 'c) wrapper

  include Unsafe3(struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) wrapper
      let service obj request = M.service obj.cell request
    end)

  let inj cell = {cell; marker}
  let prj t = t.cell
end
