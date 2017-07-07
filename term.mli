type t =
  | Variable of Variable.t
  | Function of Function.t * t list

(* instaciated = instance var with in *)
val instance : Variable.t -> t -> t -> t
