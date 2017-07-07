type t =
  | Not       of t
  | And       of t   * t
  | Or        of t   * t
  | Implies   of t   * t
  (* quantified variable, formula *)
  | Exists    of Variable.t * t
  | ForAll    of Variable.t * t
  (* predicate, aguments *)
  | Predicate of Predicate.t * Term.t list

(* instaciated = instance var with in *)
val instance : Variable.t -> Term.t -> t -> t
