type t =
  | Not       of t
  | And       of t   * t
  | Or        of t   * t
  | Implies   of t   * t
  (* quantified variable, formula *)
  | Exists    of VarSymb.t * t
  | ForAll    of VarSymb.t * t
  (* predicate, aguments *)
  | Predicate of PredSymb.t * Term.t list

let rec to_string = function
  | Not f -> "¬" ^ to_string f ^ ""
  | And (a,b) ->
      "(" ^ to_string a ^ " ∧ " ^ to_string b ^ ")"
  | Or  (a,b) ->
      "(" ^ to_string a ^ " ∨ " ^ to_string b ^ ")"
  | Implies (a,b) ->
      "(" ^ to_string a ^ " ⇒ " ^ to_string b ^ ")"
  | Exists (x, f) -> "∃" ^ VarSymb.to_string x ^ "." ^ to_string f
  | ForAll (x, f) -> "∀" ^ VarSymb.to_string x ^ "." ^ to_string f
  | Predicate (p, args) ->
      match List.length args with
      | 0 -> PredSymb.to_string p
      | _ ->
          let a = List.map Term.to_string args |> String.concat ", " in
          PredSymb.to_string p ^ "(" ^ a ^ ")"

let free_vars formula =
  let open VarSet in
  let fv = ref empty in
  let rec r bound = function
    | Not      f     -> r bound f;
    | And     (a, b) -> r bound a; r bound b;
    | Or      (a, b) -> r bound a; r bound b;
    | Implies (a, b) -> r bound a; r bound b;
    (* bind variables *)
    | Exists  (x, f) -> r (add x bound) f;
    | ForAll  (x, f) -> r (add x bound) f;
    (* predicate argument instanciation *)
    | Predicate (p, args) ->
        let f arg =
          let free = Term.free_vars bound arg in
          fv := union free !fv;
        in
        List.iter f args;
  in
  r empty formula;
  !fv
