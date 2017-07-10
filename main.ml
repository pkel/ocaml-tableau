open Term
open Formula
open Tableau

(*
 * Helper
 *)

let fresh_const name =
  let f = FunSymb.fresh 0 name in
  Term.Function (f, [])

let fresh_pred_const name =
  let p = PredSymb.fresh 0 name in
  Formula.Predicate (p, [])

let fresh_var name =
  Term.Variable (VarSymb.fresh name)

(*
 * Propositional Logic
 *)

let x = fresh_pred_const "x"
let y = fresh_pred_const "y"

let one = Or (And (x,y), Not (x))

let two =
  let l = x
  and r = Or (x, y)
  in Implies (l, r)

let check_ ff power formulas =
  print_endline "";
  let f = List.map to_string formulas |> String.concat "; " in
  print_endline ("Start Tableau: " ^ f);
  print_endline "" ;
  let tab = init power formulas in
  match ff tab |> state with
  | Working  -> raise (Failure "Program Logic")
  | DeadEnd  -> print_endline  "Reached dead end."
  | Aborted  -> print_endline ("Stopped after " ^
                               string_of_int power ^ " gammas." )
  | Closed i -> print_endline ("Tableau closed after " ^
                               string_of_int i ^ " gammas." )

let check_verbose =
  check_ verbose_expand

let check =
  check_ expand

let () =
  check_verbose 10 [Not one];
  check_verbose 10 [Not two]

(*
 * First Order Exam Exercise
 *)

let x = VarSymb.fresh "x"
let y = VarSymb.fresh "y"
let z = VarSymb.fresh "z"
let w = VarSymb.fresh "w"
let p = PredSymb.fresh 1 "P"
let q = PredSymb.fresh 1 "Q"
let r = PredSymb.fresh 1 "R"
let p x = Predicate (p, [Variable x])
let q x = Predicate (q, [Variable x])
let r x = Predicate (r, [Variable x])

let foExamExample =
  Exists(x,
  ForAll(y,
  ForAll(z,
  ForAll(w,
  Implies(
    Or(p y, Or(q z, r w)),
    Or(p x, Or(q x, r x))
  )))))

let () =
  check_verbose 100 [Not foExamExample]

(*
 * Who killed aunt Agatha?
 *)

let const name = Function (FunSymb.fresh  0 name, [])
let pred1 name =
  let p = PredSymb.fresh 1 name in
  fun x   -> Predicate (p, [x])
let pred2 name =
  let p = PredSymb.fresh 2 name in
  fun x y -> Predicate (p, [x; y])

let agatha  = const "agatha"
let butler  = const "butler"
let charles = const "charles"

let lives  = pred1 "lives"
let killed = pred2 "killed"
let richer = pred2 "richer"
let hates  = pred2 "hates"

let x_ = VarSymb.fresh "x"
let x  = Variable x_
let y_ = VarSymb.fresh "y"
let y  = Variable y_


let conjunction lst =
  let f a b = And(a,b) in
  match lst with
  | [] -> raise (Failure "Can't build empty formula")
  | hd::[] -> hd
  | hd::tl -> List.fold_left f hd tl


let triv = Implies (ForAll (x_, lives x), lives agatha)

let () =
  check_verbose 10 [Not triv]

let axioms =
  [ ForAll (x_,
      ForAll (y_,
        Implies (killed x y, hates x y)
    ))
  ; ForAll (x_,
      ForAll (y_,
        Implies (killed x y, Not (richer x y))
    ))
  ; ForAll (x_,
      Implies (hates agatha x, Not(hates charles x)
    ))
  ; hates agatha charles
  ; hates agatha agatha
  ; ForAll (x_,
      Implies (Not (richer x agatha), hates butler x)
    )
  (*
  ; ForAll (x_,
      Implies (And (lives x, Not (richer x agatha)), hates butler x)
    )
  *)
  ; ForAll (x_,
      Implies (hates agatha x, hates butler x)
    )
  ; ForAll (x_,
      Not ( conjunction
        [ hates x agatha
        ; hates x butler
        ; hates x charles ] )
    )
  (*
  ; lives agatha
  ; lives butler
  ; lives charles
  *)
  ]

let conjecture1 = Not (killed butler  agatha)
let conjecture2 = Not (killed charles agatha)
let conjecture  = And (conjecture1, conjecture2)

let () =
  check 100 (Not conjecture1 :: axioms);
  check 100 (Not conjecture2 :: axioms);
  check 300 (Not conjecture  :: axioms)
