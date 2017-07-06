open Formula
open Prover

let x = fresh_var ()

let y = fresh_var ()

let one = Or (And (x,y), Neg (x))

let two =
  let l = x
  and r = Or (x, y)
  in Impl (l, r)

let _ =
  match Prover.tableau(one) with
  | None -> print_string "First formula valid\n"
  | Some x -> print_string "First fomrula invalid\n"

let _ =
  match Prover.tableau(two) with
  | None -> print_string "Second formula valid\n"
  | Some x -> print_string "Second formula invalid\n"
