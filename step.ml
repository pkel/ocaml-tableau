type t =
  | Literal   of Formula.t
  | DoubleNeg of Formula.t
  | Alpha     of Formula.t * Formula.t
  | Beta      of Formula.t * Formula.t
  | Gamma     of VarSymb.t * Formula.t
  | Delta     of VarSymb.t * Formula.t

open Formula

(* step from formula *)
let step = function
  (* double negation *)
  | Not (Not a)           -> DoubleNeg a
  (* alpha *)
  | And (a1,a2)           -> Alpha (    a1,     a2 )
  | Not (Or (a1,a2))      -> Alpha (Not a1, Not a2 )
  | Not (Implies (a1,a2)) -> Alpha (    a1, Not a2 )
  (* beta *)
  | Or (b1,b2)            -> Beta  (    b1,     b2 )
  | Not (And (b1,b2))     -> Beta  (Not b1, Not b2 )
  | Implies (b1,b2)       -> Beta  (Not b1,     b2 )
  (* gamma *)
  | ForAll (v,f)          -> Gamma (v,     f )
  | Not (Exists (v,f))    -> Gamma (v, Not f )
  (* delta *)
  | Exists (v,f)          -> Delta (v,     f )
  | Not (ForAll (v,f))    -> Delta (v, Not f )
  (* not possible *)
  | a                     -> Literal   a

let apply subst t =
  let s = subst in
  let f = Substitution.apply_formula in
  match t with
  | Alpha (a,b) -> Alpha (f s a, f s b)
  | Beta  (a,b) -> Beta  (f s a, f s b)
  | Gamma (v,a) -> let s = Substitution.remove v s in Gamma (v, f s a)
  | Delta (v,a) -> let s = Substitution.remove v s in Delta (v, f s a)
  | DoubleNeg a -> DoubleNeg (f s a)
  | Literal   a -> Literal   (f s a)

