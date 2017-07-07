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
  match tableau(one) with
  | None -> print_string "First formula valid\n"
  | Some x -> print_string "First formula invalid\n"

let _ =
  match tableau(two) with
  | None -> print_string "Second formula valid\n"
  | Some x -> print_string "Second formula invalid\n"
