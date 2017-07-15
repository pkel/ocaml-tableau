open Step

type t =
  { precedence : int
  ; id : int
  ; step : Step.t
  }

(* assign each step an initial precedence *)
let precedence = function
  | Literal   _ -> 0
  | DoubleNeg _ -> 1
  | Alpha     _ -> 2
  | Beta      _ -> 3
  | Delta     _ -> 4
  | Gamma     _ -> 5

(* mutable counter to index formulas *)
let count = ref 0

(* new working formula increases counter *)
let create formula =
  count := !count + 1;
  let step = step (formula) in
  { precedence = precedence step
  ; id = !count
  ; step = step
  }

let apply subst t =
  { t with step = Step.apply subst t.step }

let to_string { step } = Step.to_string step

let step { step } = step

(* min_elt on set will gives lowest precedence and lowest id *)
let compare t1 t2 =
  match Pervasives.compare t1.precedence t2.precedence with
  | 0 -> Pervasives.compare t1.id t2.id
  | x -> x

(* what happens to precedence on usage? *)
let use t = { t with precedence = t.precedence + 10 }


