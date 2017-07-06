type var = int

type t =
  | Var   of var
  | Neg   of t
  | And   of t * t
  | Or    of t * t
  | Impl  of t * t
  (* quantifier *)
  | Exist of var * t
  | All   of var * t

(* variable *)
let var_count = ref 0

let fresh_var () =
  var_count := !var_count + 1;
  Var !var_count

(* quantifier instanciation *)
let instance var term formula =
  let rec r = function
    (* instanciate *)
    | Var    x     -> if x = var then term else Var x
    (* recurse *)
    | Neg    f     -> r f
    | And   (a, b) -> And  (r a, r b)
    | Or    (a, b) -> Or   (r a, r b)
    | Impl  (a, b) -> Impl (r a, r b)
    (* don't replace bound variables *)
    | Exist (x, f) -> if x = var then Exist (x, f) else Exist (x, r f)
    | All   (x, f) -> if x = var then All   (x, f) else All   (x, r f)
  in
  r formula
