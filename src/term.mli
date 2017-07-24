type t =
  | Variable of Symbol.t
  | Function of Symbol.t * t list

val to_string : t -> string

(* instaciated = instance var with in *)
val instance : Symbol.t -> t -> t -> t

(* free_in_term = free_vars bound term *)
val free_vars : VarSet.t -> t -> VarSet.t


