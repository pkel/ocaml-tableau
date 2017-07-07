type t =
  | Not       of t
  | And       of t   * t
  | Or        of t   * t
  | Implies   of t   * t
  (* quantified variable, formula *)
  | Exists    of Variable.t * t
  | ForAll    of Variable.t * t
  (* predicate, aguments *)
  | Predicate of Predicate.t * Term.t list

(* variable instanciation *)
let instance var term formula =
  let rec r = function
    | Not      f     -> r f
    | And     (a, b) -> And     (r a, r b)
    | Or      (a, b) -> Or      (r a, r b)
    | Implies (a, b) -> Implies (r a, r b)
    (* don't replace bound variables *)
    | Exists  (x, f) -> if x = var then Exists (x, f) else Exists (x, r f)
    | ForAll  (x, f) -> if x = var then ForAll (x, f) else ForAll (x, r f)
    (* predicate argument instanciation *)
    | Predicate (p, args) ->
        let f = Term.instance var term in
        Predicate (p, List.map f args)
  in
  r formula
