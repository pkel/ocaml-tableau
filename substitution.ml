module M = Map.Make(VarSymb)

type t = Term.t M.t

open Term

let empty = M.empty

let add = M.add

let remove = M.remove

let occurs x term =
  (* TODO: Set is  clear overhead *)
  Term.free_vars VarSet.empty term |> VarSet.mem x

let apply_var t v =
  try M.find v t
  with Not_found -> Variable v

let rec apply_term subst = function
  (* instanciate *)
  | Variable x -> apply_var subst x
  (* recurse *)
  | Function (f, args) -> Function (f, List.map (apply_term subst) args)

let apply_formula subst formula =
  let open Formula in
  let rec r s = function
    | Not      a     -> Not     (r s a)
    | And     (a, b) -> And     (r s a, r s b)
    | Or      (a, b) -> Or      (r s a, r s b)
    | Implies (a, b) -> Implies (r s a, r s b)
    (* don't replace bound variables *)
    | Exists  (x, f) -> let s = remove x s in Exists (x, r s f)
    | ForAll  (x, f) -> let s = remove x s in ForAll (x, r s f)
    (* predicate argument instanciation *)
    | Predicate (p, args) ->
        let f = apply_term s in
        Predicate (p, List.map f args)
  in
  r subst formula

(* Unification of terms *)

exception SymbolClash
exception Occurs

let unifier lst =
  let zip = List.map2 (fun x y -> x,y) in
  let substitute x term =
    let s = add x term empty in
    let f = apply_term s in
    let g (a,b) = f a, f b in
    List.map g
  in
  let rec r subst = function
    (* done *)
    | [] -> Some subst
    (* unify first pair of terms *)
    | hd::tl ->
        match hd with
        (* unify function by unifying all arguments *)
        | Function (f, fargs) , Function (g, gargs) ->
            if FunSymb.compare f g = 0 && FunSymb.arity f = FunSymb.arity g
            then zip fargs gargs |> List.rev_append tl |> r subst
            else raise SymbolClash ;
        (* unify with variable by substitution *)
        | Variable x, Variable y ->
            if VarSymb.compare x y = 0 then r subst tl
            else substitute x (Variable y) tl |>
              r (add x (Variable y) subst)
        | Variable x, term
        | term, Variable x ->
            if occurs x term then raise Occurs
            else substitute x term tl |> r (add x term subst)
  in
  try r empty lst with
  | SymbolClash -> None
  | Occurs      -> None


