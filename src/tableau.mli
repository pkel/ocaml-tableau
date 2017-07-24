type state =
  | Working
  | DeadEnd
  | Aborted
  | Closed of int

type t

val state : t -> state

val print : t -> unit

(* fresh_tableau = init gammaLimit formulas *)
val init : int -> Formula.t list -> t

val step : t -> t

(* step till done *)
val expand : t -> t

val verbose_expand : t -> t


