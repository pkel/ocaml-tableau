type state =
  | Working
  | DeadEnd
  | Aborted
  | Closed

type t

val state : t -> state

(* fresh_tableau = init gammaLimit formulas *)
val init : int -> Formula.t list -> t

val step : t -> t

(* step till done *)
val expand : t -> t

