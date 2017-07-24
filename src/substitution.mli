type t

val empty : t

val add : Symbol.t -> Term.t -> t -> t

val remove : Symbol.t -> t -> t

val unifier : (Term.t * Term.t) list -> t option

val apply_var: t -> Symbol.t -> Term.t
val apply_term : t -> Term.t -> Term.t
val apply_formula: t -> Formula.t -> Formula.t

