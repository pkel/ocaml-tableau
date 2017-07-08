open Formula

module C = Convenience

type step =
  | Literal   of Formula.t
  | DoubleNeg of Formula.t
  | Alpha     of Formula.t * Formula.t
  | Beta      of Formula.t * Formula.t
  | Gamma     of VarSymb.t * Formula.t
  | Delta     of VarSymb.t * Formula.t

type branch  = Formula.t list
type tableau = branch  list

(* step from formula *)
let step = function
  (* double negation *)
  | Not(Not(a))         -> DoubleNeg a
  (* alpha *)
  | And(a1,a2)          -> Alpha     (a1,      a2)
  | Not(Or(a1,a2))      -> Alpha     (Not(a1), Not(a2))
  | Not(Implies(a1,a2)) -> Alpha     (a1,      Not(a2))
  (* beta *)
  | Or(b1,b2)           -> Beta      (b1,      b2)
  | Not(And(b1,b2))     -> Beta      (Not(b1), Not(b2))
  | Implies(b1,b2)      -> Beta      (Not(b1), b2)
  (* gamma *)
  | ForAll(v,f)         -> Gamma     (v,       f)
  | Not(Exists(v,f))    -> Gamma     (v,       Not(f))
  (* delta *)
  | Exists(v,f)         -> Delta     (v,       f)
  | Not(ForAll(v,f))    -> Delta     (v,       Not(f))
  (* not possible *)
  | a                   -> Literal   a

(* assign each step an initial precedence *)
let precedence formula =
  match step(formula) with
  | Literal   _ -> 0
  | DoubleNeg _ -> 1
  | Alpha     _ -> 2
  | Beta      _ -> 3
  | Delta     _ -> 4
  | Gamma     _ -> 5

(* working formula *)
module WF =
  struct
    (* precedence * id * formula itself *)
    type t = int * int * Formula.t

    (* mutable counter to index formulas *)
    let count = ref 0

    (* new working formula increases counter *)
    let create formula =
      count := !count + 1;
      precedence(formula), !count, formula

    (* what happens to precedence on usage? *)
    let use (p, id, formula) = (p + 10, id, formula)

    (* min_elt on set will gives lowest precedence and lowest id *)
    let compare (p1,i1,_) (p2,i2,_) =
      match Pervasives.compare p1 p2 with
      | 0 -> Pervasives.compare i1 i2
      | x -> x
  end

module WFS = Set.Make(WF)

(* formula with identifier and precedence *)
type working_formula = WF.t
(* literals , (ordered) set of available formulas *)
type working_branch  = Formula.t list * WFS.t
type working_tableau = working_branch list

(* top formula from set *)
let peek set =
  try
    let _, _, formula = WFS.min_elt set in
    Some formula
  with Not_found -> None

(* delete top element of set *)
let drop set =
  try
    let top = WFS.min_elt set in
    WFS.remove top set
  with Not_found -> set

(* keep top element but mark as used *)
let keep set =
  try
    let top = WFS.min_elt set in
    set |> WFS.add top |> WFS.add (WF.use top)
  with Not_found -> set

(* add formula to set of working formulas *)
let add formula set =
  let wf = WF.create(formula) in
  WFS.add wf set

(* singleton set *)
let singleton formula =
  WFS.empty |> add formula

(* find unclosable branch *)
let tableau formula =
  let closure literals = function
    | Not lit -> List.exists ((=)      lit ) literals
    |     lit -> List.exists ((=) (Not lit)) literals
  in
  let rec expand = function
    (* match unexpanded branches *)
    | [] -> None
    | (literals, formulas)::tl ->
        match peek(formulas) with
        | None -> Some literals
        | Some formula ->
            match step(formula) with
            | Alpha(a1,a2) ->
                (* expand branch, delete source formula *)
                let b = (literals, drop formulas |> add a1 |> add a2) in
                expand (b::tl)
            | Beta(b1,b2) ->
                (* split branch, delete source formula *)
                let l = (literals, drop formulas |> add b1) in
                let r = (literals, drop formulas |> add b2) in
                expand (l::r::tl)
            | Gamma(x, f) ->
                (* fresh variable v, x->v, keep gamma *)
                let b = (literals, keep formulas |> add (
                  instance x (C.fresh_var "") f
                  )) in
                expand (b::tl)
            | Delta(x, f) ->
                (* fresh skolem function sk, x->sk, drop delta *)
                let args = free_vars formula |> VarSet.elements |>
                  List.map (fun x -> Term.Variable x)
                in
                let n = List.length args in
                let sk = Term.Function (FunSymb.fresh n "", args) in
                let b = (literals, drop formulas |>  add (instance x sk f)) in
                expand (b::tl)
            | DoubleNeg a ->
                (* drop double neg *)
                let b = (literals, drop formulas |> add a) in
                expand (b::tl)
            | Literal a ->
                (* check whether found literal closes the branch *)
                match closure literals a with
                | true -> expand tl
                | false ->
                    (* expand list of literals, go on with branch *)
                    let b = (a::literals, drop formulas) in
                    expand (b::tl)
  in
  expand [ [], singleton (Not formula) ]


