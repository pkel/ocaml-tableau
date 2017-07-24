type t =
  | Variable of VarSymb.t
  | Function of FunSymb.t * t list

let rec to_string = function
  | Variable x -> VarSymb.to_string x
  | Function (f, args) ->
      match List.length args with
      | 0 -> FunSymb.to_string f
      | _ ->
          let a = List.map to_string args |> String.concat ", " in
          FunSymb.to_string f ^ "(" ^ a ^ ")"

let instance var term within =
  let rec r = function
    (* instanciate *)
    | Variable x -> if x = var then term else Variable x
    (* recurse *)
    | Function (f, args) -> Function (f, List.map r args)
  in
  r within

let free_vars bound term =
  let open VarSet in
  let fv = ref empty in
  let rec r = function
    | Variable  x        -> if mem x bound then () else fv := add x !fv;
    | Function (_, args) -> List.iter r args
  in
  r term;
  !fv



