type t =
  | Not       of t
  | And       of t   * t
  | Or        of t   * t
  | Implies   of t   * t
  (* quantified variable, formula *)
  | Exists    of VarSymb.t * t
  | ForAll    of VarSymb.t * t
  (* predicate, aguments *)
  | Predicate of PredSymb.t * Term.t list

val free_vars : t -> VarSet.t

val to_string : t -> string