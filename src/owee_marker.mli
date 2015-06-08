type 'result service = ..

type _ service +=
   | Name : string service
   | Traverse : ((Obj.t -> 'acc -> 'acc) -> 'acc -> 'acc) service
   | Locate : Owee_location.t list service

type 'a service_result =
  | Success of 'a
  | Unsupported_service
  | Unmanaged_object

module type T0 = sig
  type t
  val service : t -> 'result service -> 'result service_result
end
val query_service : 'a -> 'result service -> 'result service_result

type 'a marked
val get : 'a marked -> 'a
module Safe0 (M : T0) : sig
  val mark : M.t -> M.t marked
end

type 'a marker
module Unsafe0 (M : T0) : sig
  val marker : M.t marker
end

(* Generalization *)

module type T1 = sig
  type 'x t
  val service : 'x t -> 'result service -> 'result service_result
end
module type T2 = sig
  type ('x, 'y) t
  val service : ('x, 'y) t -> 'result service -> 'result service_result
end
module type T3 = sig
  type ('x, 'y, 'z) t
  val service : ('x, 'y, 'z) t -> 'result service -> 'result service_result
end

module Safe1 (M : T1) : sig
  val mark : 'a M.t -> 'a M.t marked
end
module Safe2 (M : T2) : sig
  val mark : ('a, 'b) M.t -> ('a, 'b) M.t marked
end
module Safe3 (M : T3) : sig
  val mark : ('a, 'b, 'c) M.t -> ('a, 'b, 'c) M.t marked
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
