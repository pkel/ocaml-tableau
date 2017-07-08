type t =
  | Variable of VarSymb.t
  | Function of FunSymb.t * t list

(* instaciated = instance var with in *)
val instance : VarSymb.t -> t -> t -> t

(* free_in_term = free_vars bound term *)
val free_vars : VarSet.t -> t -> VarSet.t

val unifiable : t -> t -> bool
