type var

type t =
  | Var   of var
  | Neg   of t
  | And   of t   * t
  | Or    of t   * t
  | Impl  of t   * t
  | Exist of var * t
  | All   of var * t

val fresh_var : unit -> t

(* instaciated = instance var with in *)
val instance : var -> t -> t -> t
