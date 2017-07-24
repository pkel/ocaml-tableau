type t

(* function = fresh arity name *)
val fresh : int -> string -> t

val arity : t -> int

val to_string : t -> string

val compare : t -> t -> int

