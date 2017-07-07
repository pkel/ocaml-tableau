type t =
  | Variable of Variable.t
  | Function of Function.t * t list

(* instaciated = instance var with in *)
let instance var term within =
  let rec r = function
    (* instanciate *)
    | Variable x -> if x = var then term else Variable x
    (* recurse *)
    | Function (f, args) -> Function (f, List.map r args)
  in
  r within

