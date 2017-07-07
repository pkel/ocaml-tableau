let fresh_const name =
  let f = FunSymb.fresh 0 name in
  Term.Function (f, [])

let fresh_pred_const name =
  let p = PredSymb.fresh 0 name in
  Formula.Predicate (p, [])

let fresh_var name =
  Term.Variable (VarSymb.fresh name)
