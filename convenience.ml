let fresh_const name =
  let p = Predicate.fresh 0 name in
  Formula.Predicate (p, [])

let fresh_var name =
  Term.Variable (Variable.fresh name)
