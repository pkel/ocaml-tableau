type t =
  | Literal   of Formula.t
  | DoubleNeg of Formula.t
  | Alpha     of Formula.t * Formula.t
  | Beta      of Formula.t * Formula.t
  | Gamma     of VarSymb.t * Formula.t
  | Delta     of VarSymb.t * Formula.t

(* step from formula *)
val step : Formula.t -> t
