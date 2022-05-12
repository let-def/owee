type 'a interval = {
  lbound: int;
  rbound: int;
  value: 'a;
}

val interval : int64 -> int64 -> 'a -> 'a interval

type 'a t

(** {4 Constructors} *)

(** [create count f] : returns a new interval map which contains [count] intervals,
    returned by [f i] where [i] is from 0 to [count - 1].

    @raise Invalid_argument if [count < 0] or [count > Sys.max_array_length]. *)
val create : int -> f:(int -> 'a interval) -> 'a t

(** {4 Query} *)

(** [query t q] : list of intervals in [t] containing the Int64.t [q] *)
val query : 'a t -> Int64.t -> 'a interval list

(** [iter t ~f] applies [f] to each interval that has been added to
    [t].  Traversal order is not specified.  *)
val iter : 'a t -> f:('a interval -> unit) -> unit
