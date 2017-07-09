open Formula
open Prover
open Convenience

(* let x = fresh_var "x" *)
(* let y = fresh_var "y" *)
let x = fresh_pred_const "x"
let y = fresh_pred_const "y"

let one = Or (And (x,y), Not (x))

let two =
  let l = x
  and r = Or (x, y)
  in Implies (l, r)

let _ =
  match tableau 10 one with
  | None -> print_string "First formula valid\n"
  | Some x -> print_string "First formula invalid\n"

let _ =
  match tableau 10 two with
  | None -> print_string "Second formula valid\n"
  | Some x -> print_string "Second formula invalid\n"


let x = VarSymb.fresh "x"
let y = VarSymb.fresh "y"
let z = VarSymb.fresh "z"
let w = VarSymb.fresh "w"
let p = PredSymb.fresh 1 "P"
let q = PredSymb.fresh 1 "Q"
let r = PredSymb.fresh 1 "R"
let p x = Predicate (p, [Term.Variable x])
let q x = Predicate (q, [Term.Variable x])
let r x = Predicate (r, [Term.Variable x])

let foExamExample =
  Exists(x,
  ForAll(y,
  ForAll(z,
  ForAll(w,
  Implies(
    Or(p y, Or(q z, r w)),
    Or(p x, Or(q x, r x))
  )))))

let _ =
  match tableau 100 foExamExample with
  | None -> print_endline "Found tableau prove for f.o. exam example."
  | _    -> print_endline "Did not find tableau prove for f.o. exam example."
