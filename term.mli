type t =
  | Variable of VarSymb.t
  | Function of FunSymb.t * t list

(* instaciated = instance var with in *)
val instance : VarSymb.t -> t -> t -> t
