type formula =
  | And    of formula * formula
  | Or     of formula * formula
  | Impl   of formula * formula
  | Neg    of formula
  | Var    of int

type step =
  | Beta      of formula * formula
  | Alpha     of formula * formula
  | DoubleNeg of formula
  | Literal   of formula

type branch  = formula list
type tableau = branch  list

(* step from formula *)
let step = function
  (* alpha *)
  | And(a1, a2)       -> Alpha(a1     , a2     )
  | Neg(Or(a1, a2))   -> Alpha(Neg(a1), Neg(a2))
  | Neg(Impl(a1, a2)) -> Alpha(a1     , Neg(a2))
  (* beta *)
  | Or(b1, b2)        -> Beta(b1     , b2     )
  | Neg(And(b1, b2))  -> Beta(Neg(b1), Neg(b2))
  | Impl(b1, b2)      -> Beta(Neg(b1), b2     )
  (* double negation *)
  | Neg(Neg(a))       -> DoubleNeg a
  (* not possible *)
  | a                 -> Literal a

(* assign each step an initial precedence *)
let precedence formula =
  match step(formula) with
  | Beta      _ -> 3
  | Alpha     _ -> 2
  | DoubleNeg _ -> 1
  | Literal   _ -> 0

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
    let use (p, id, formula) = (p + 1, id, formula)

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

(* example formulas. A parser or infix notation would help a lot *)
let one = Or (And (Var 1, Var 2), Neg (Var 1))
let two =
  let l = Var 1
  and r = Or (Var 1, Var 2)
  in Impl (l, r)
