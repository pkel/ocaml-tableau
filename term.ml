type t =
  | Variable of VarSymb.t
  | Function of FunSymb.t * t list


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

exception SymbolClash
exception Occurs

let occurs x term =
  (* TODO: Set is  clear overhead *)
  free_vars VarSet.empty term |> VarSet.mem x

let unifiable lst =
  let zip = List.map2 (fun x y -> x,y) in
  let substitute x term =
    let f = instance x term in
    let g (a,b) = f a, f b in
    List.map g
  in
  let rec r = function
    (* done *)
    | [] -> true
    (* unify first pair of terms *)
    | hd::tl ->
        match hd with
        (* unify function by unifying all arguments *)
        | Function (f, fargs) , Function (g, gargs) ->
            if FunSymb.compare f g = 0 && FunSymb.arity f = FunSymb.arity g
            then zip fargs gargs |> List.rev_append tl |> r
            else raise SymbolClash ;
        (* unify with variable by substitution *)
        | Variable x, Variable y ->
            if VarSymb.compare x y = 0 then r tl
            else substitute x (Variable y) tl |> r
        | Variable x, term
        | term, Variable x ->
            if occurs x term then raise Occurs
            else substitute x term tl |> r
  in
  try r lst with
  | SymbolClash
  | Occurs -> false


