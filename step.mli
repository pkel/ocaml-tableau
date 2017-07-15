type t =
  | Literal   of Formula.t
  | DoubleNeg of Formula.t
  | Alpha     of Formula.t * Formula.t
  | Beta      of Formula.t * Formula.t
  | Gamma     of VarSymb.t * Formula.t
  | Delta     of VarSymb.t * Formula.t

val to_string : t -> string

(* step from formula *)
val step : Formula.t -> t

val apply : Substitution.t -> t -> t
