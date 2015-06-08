type 'result service = ..

type _ service +=
   | Name : string service
   | Traverse : ((Obj.t -> 'acc -> 'acc) -> 'acc -> 'acc) service

module type T0 = sig
  type t
  val service : t -> 'result service -> 'result
end
val query_service : 'a -> 'result service -> 'result
exception Unmanaged_object
exception Unsupported_service

module Safe0 (M : T0) : sig
  type t
  val inj : M.t -> t
  val prj : t -> M.t
end

type 'a marker

module Unsafe0 (M : T0) : sig
  val marker : M.t marker
end

(* Generalization *)

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
end
module Unsafe2 (M : T2) : sig
  val marker : ('x, 'y) M.t marker
end
module Unsafe3 (M : T3) : sig
  val marker : ('x, 'y, 'z) M.t marker
end
module Safe1 (M : T1) : sig
  type 'a t
  val inj : 'a M.t -> 'a t
  val prj : 'a t -> 'a M.t
end
module Safe2 (M : T2) : sig
  type ('a, 'b) t
  val inj : ('a, 'b) M.t -> ('a, 'b) t
  val prj : ('a, 'b) t -> ('a, 'b) M.t
end
module Safe3 (M : T3) : sig
  type ('a, 'b, 'c) t
  val inj : ('a, 'b, 'c) M.t -> ('a, 'b, 'c) t
  val prj : ('a, 'b, 'c) t -> ('a, 'b, 'c) M.t
end
