type t =
  | Literal   of Formula.t
  | DoubleNeg of Formula.t
  | Alpha     of Formula.t * Formula.t
  | Beta      of Formula.t * Formula.t
  | Gamma     of Symbol.t  * Formula.t
  | Delta     of Symbol.t  * Formula.t

val to_string : t -> string

(* step from formula *)
val step : Formula.t -> t

val apply : Substitution.t -> t -> t
