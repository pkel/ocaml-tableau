type state =
  | Closed
  | Open

type t

(* add formula to branch, try to close if literal *)
val add : Formula.t -> t -> t

val literals : t -> Formula.t list

(* get next formula on branch according to precedence *)
val peek: t -> Formula.t option

(* keep/drop the peeked formula dependent on formula step *)
val consume: t -> t

val singleton: Formula.t -> t

val of_list: Formula.t list -> t

val state: t -> state

