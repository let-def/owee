type t
val extract : (_ -> _) -> t
val lookup : t -> (string * int * int) option

val locate : (_ -> _) -> (string * int * int) option
