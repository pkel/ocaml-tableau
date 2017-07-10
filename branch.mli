type t

val print : t -> unit

(* add formula to branch, try to close if literal *)
val add : Formula.t -> t -> t

(* get next formula on branch according to precedence *)
val peek: t -> Step.t option

(* keep/drop the peeked formula dependent on formula step *)
val consume: t -> t

val singleton: Formula.t -> t

val of_list: Formula.t list -> t

(* Some subst = closure newlit branch *)
val closure: Formula.t -> t -> Substitution.t option

val apply: Substitution.t -> t -> t
