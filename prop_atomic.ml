type term =
  | And    of term * term
  | Or     of term * term
  | Impl   of term * term
  | Neg    of term
  | Var    of int


type step =
  | Beta      of term * term
  | Alpha     of term * term
  | DoubleNeg of term
  | Literal   of term


type tableau = term list list


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


(* full, strict expansion *)
let tableau formula =
  (* acc = fully expanded branches *)
  let rec expand acc = function
    (* match unexpanded branches *)
    | [] -> acc
    | hdBr::tlBr ->
        match hdBr with
        (* match list of unexpanded formulas in first branch *)
        | literals, [] -> expand (literals::acc) tlBr
        | literals, hd::tl ->
            match step(hd) with
            | Alpha(a1,a2) ->
                (* expand branch, delete source formula *)
                let b = (literals, a1::a2::tl) in
                expand acc (b::tlBr)
            | Beta(b1,b2) ->
                (* split branch, delete source formula *)
                let l = (literals, b1::tl) in
                let r = (literals, b2::tl) in
                expand acc (l::r::tlBr)
            | DoubleNeg a ->
                (* drop double neg *)
                let b = (literals, a::tl) in
                expand acc (b::tlBr)
            | Literal a ->
                (* expand list of literals *)
                let b = (a::literals, tl) in
                expand acc (b::tlBr)
  in
  expand [] [[],[Neg(formula)]]


(* find a branch in tableau that cannot be closed *)
let rec openBranch tableau =
  (* find conflict within branch *)
  let rec closable lst = function
    | []  -> false
    | hd::tl ->
        let closes =
          match hd with
          | Neg(x) -> (=) x
          | x ->      (=) (Neg(x))
        in
        match List.exists closes lst with
        | true -> true
        | false -> closable (hd::lst) tl
  in
  match tableau with
  | [] -> None
  | hd::tl ->
      match closable [] hd with
      | true  -> openBranch tl
      | false -> Some hd


(* close branches on the fly *)
let tableau_otf formula =
  let closure literals = function
    | Neg lit -> List.exists ((=)      lit ) literals
    |     lit -> List.exists ((=) (Neg lit)) literals
  in
  let rec expand = function
    (* match unexpanded branches *)
    | [] -> None
    | hdBr::tlBr ->
        match hdBr with
        | literals, [] -> Some literals
        | literals, hd::tl ->
            match step(hd) with
            | Alpha(a1,a2) ->
                (* expand branch, delete source formula *)
                let b = (literals, a1::a2::tl) in
                expand (b::tlBr)
            | Beta(b1,b2) ->
                (* split branch, delete source formula *)
                let l = (literals, b1::tl) in
                let r = (literals, b2::tl) in
                expand (l::r::tlBr)
            | DoubleNeg a ->
                (* drop double neg *)
                let b = (literals, a::tl) in
                expand (b::tlBr)
            | Literal a ->
                (* check whether found literal closes the branch *)
                match closure literals a with
                | true -> expand tlBr
                | false ->
                    (* expand list of literals, go on with branch  *)
                    let b = (a::literals, tl) in
                    expand (b::tlBr)
  in
  expand [[],[Neg(formula)]]


let one = Or(And(Var(1), Var(2)), Neg(Var(1)))
let two =
  let l = Var(1)
  and r = Or(Var(1), Var(2))
  in Impl(l,r)

