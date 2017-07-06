module Formula =
  struct
    type variable = int

    type t =
      | Var   of variable
      | Neg   of t
      | And   of t * t
      | Or    of t * t
      | Impl  of t * t
      (* quantifier *)
      | Exist of variable * t
      | All   of variable * t

    (* variable *)
    let var_count = ref 0

    let fresh_var () =
      var_count := !var_count + 1;
      Var !var_count

    (* quantifier instanciation *)
    let instance variable term formula =
      let rec r = function
        (* instanciate *)
        | Var    x     -> if x = variable then term else Var x
        (* recurse *)
        | Neg    f     -> r f
        | And   (a, b) -> And  (r a, r b)
        | Or    (a, b) -> Or   (r a, r b)
        | Impl  (a, b) -> Impl (r a, r b)
        (* don't replace bound variables *)
        | Exist (x, f) -> if x = variable then Exist (x, f) else Exist (x, r f)
        | All   (x, f) -> if x = variable then All   (x, f) else All   (x, r f)
      in
      r formula
      end

type formula = Formula.t

open Formula

type step =
  | Literal   of formula
  | DoubleNeg of formula
  | Alpha     of formula * formula
  | Beta      of formula * formula
  | Gamma     of variable * formula
  | Delta     of variable * formula

type branch  = formula list
type tableau = branch  list

(* step from formula *)
let step = function
  (* double negation *)
  | Neg(Neg(a))       -> DoubleNeg a
  (* alpha *)
  | And(a1, a2)       -> Alpha(a1     , a2     )
  | Neg(Or(a1, a2))   -> Alpha(Neg(a1), Neg(a2))
  | Neg(Impl(a1, a2)) -> Alpha(a1     , Neg(a2))
  (* beta *)
  | Or(b1, b2)        -> Beta(b1     , b2     )
  | Neg(And(b1, b2))  -> Beta(Neg(b1), Neg(b2))
  | Impl(b1, b2)      -> Beta(Neg(b1), b2     )
  (* gamma *)
  | All(v, f)         -> Gamma(v, f)
  | Neg(Exist(v,f))   -> Gamma(v, Neg(f))
  (* delta *)
  | Exist(v, f)       -> Delta(v, f)
  | Neg(All(v,f))     -> Delta(v, Neg(f))
  (* not possible *)
  | a                 -> Literal a

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
    type t = int * int * formula

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
type working_branch  = formula list * WFS.t
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
    | Neg lit -> List.exists ((=)      lit ) literals
    |     lit -> List.exists ((=) (Neg lit)) literals
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
                (* pick closed term t, x->t, keep gamma *)
                (* TODO: |> add instance of f *)
                (* not complete this way *)
                let b = (literals, keep formulas) in
                expand (b::tl)
            | Delta(x, f) ->
                (* fresh variable v, x->v, drop delta *)
                let b = (literals, drop formulas |>  add (
                  instance x (fresh_var ()) f
                  )) in
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
  expand [ [], singleton (Neg formula) ]


(* example formulas *)
let x = fresh_var ()

let y = fresh_var ()

let one = Or (And (x,y), Neg (x))

let two =
  let l = x
  and r = Or (x, y)
  in Impl (l, r)

