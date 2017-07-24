type t

val empty : t

val add : VarSymb.t -> Term.t -> t -> t

val remove : VarSymb.t -> t -> t

val unifier : (Term.t * Term.t) list -> t option

val apply_var: t -> VarSymb.t -> Term.t
val apply_term : t -> Term.t -> Term.t
val apply_formula: t -> Formula.t -> Formula.t

