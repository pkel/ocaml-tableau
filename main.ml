open Term
open Formula
open Prover

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

let check f =
  print_endline "";
  print_endline ("Check: " ^ to_string f);
  match tableau 10 f with
  | None -> print_endline "valid."
  | Some x -> print_endline "invalid."

let () =
  check one;
  check two;
  print_endline ""

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
  check foExamExample;
  print_endline ""

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

let disjunction lst =
  let f a b = Or(a,b) in
  match lst with
  | [] -> raise (Failure "Can't build empty formula")
  | hd::[] -> hd
  | hd::tl -> List.fold_left f hd tl

let puz =
  Implies ( conjunction
    [ ForAll (x_, ForAll (y_, Implies (killed x y, hates x y)))
    ; ForAll (x_, ForAll (y_, Implies (killed x y, Not (richer x y))))
    ; ForAll (x_, Implies (hates agatha x, Not(hates charles x)))
    ; hates agatha agatha
    ; hates agatha charles
    ; ForAll (x_, Implies (Not (richer x agatha), hates butler x))
    ; ForAll (x_, Implies (hates agatha x, hates butler x))
    ; ForAll (x_, disjunction
      [ Not (hates x agatha)
      ; Not (hates x butler)
      ; Not (hates x charles) ])
    (* ], And (Not (killed butler agatha), Not (killed charles agatha)) ) *)
    ], killed agatha agatha )
    (* ], killed butler agatha ) *)

let puz =
  Implies ( conjunction
    [ Implies (killed x y, hates x y)
    ; Implies (killed x y, Not (richer x y))
    ; Implies (hates agatha x, Not(hates charles x))
    ; hates agatha agatha
    ; hates agatha charles
    ; Implies (Not (richer x agatha), hates butler x)
    ; Implies (hates agatha x, hates butler x)
    ; disjunction
      [ Not (hates x agatha)
      ; Not (hates x butler)
      ; Not (hates x charles) ]
    (* ], And (Not (killed butler agatha), Not (killed charles agatha)) ) *)
    ], killed agatha agatha )
    (* ], killed butler agatha ) *)

let mini =
  Implies ( conjunction
    [ lives agatha
    ; lives butler
    ; lives charles
    ; Exists (x_, And(lives x, killed x agatha))
    ; Not (killed butler agatha)
    ; Not (killed charles agatha)
    ], killed agatha agatha )

let triv = Implies (lives agatha, lives agatha)

let () =
  check puz
