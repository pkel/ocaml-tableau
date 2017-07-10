open Step

(* assign each step an initial precedence *)
let precedence = function
  | Literal   _ -> 0
  | DoubleNeg _ -> 1
  | Alpha     _ -> 2
  | Beta      _ -> 3
  | Delta     _ -> 4
  | Gamma     _ -> 5

(* what happens to precedence on usage? *)
let use (p, id, step, formula) = (p + 10, id, step, formula)

(* Steps on branch *)
module Elt = (* TODO: make this = module Step *)
  struct
    (* precedence * id * formula itself *)
    (* TODO: use record *)
    type t = int * int * Step.t * Formula.t

    (* mutable counter to index formulas *)
    let count = ref 0

    (* new working formula increases counter *)
    let create formula =
      count := !count + 1;
      let step = step (formula) in
      precedence step, !count, step, formula

    let apply subst (p, id, step, f) =
      (p, id, Step.apply subst step, Substitution.apply_formula subst f)

    let to_string (_, _, _, f) = Formula.to_string f

    let step (_, _, s, _) = s

    (* min_elt on set will gives lowest precedence and lowest id *)
    let compare (p1,i1,_,_) (p2,i2,_,_) =
      match Pervasives.compare p1 p2 with
      | 0 -> Pervasives.compare i1 i2
      | x -> x
  end

module Set = Set.Make(Elt)

let set_map f set =
  let open Set in
  elements set |> List.map f |> of_list

(* TODO: get rid *)
let () = Random.init 42
let shuffle d =
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond


(* literals and formulas separated *)
type t =
  { lit  : Formula.t list
  ; steps : Set.t
  }


let print t =
  let lit = List.map Formula.to_string t.lit |> String.concat "; " in
  let fms = Set.elements t.steps |> List.map Elt.to_string |>
    String.concat "; "
  in
  print_endline ("    " ^ lit ^ " | " ^ fms)


let add formula t =
  { t with steps = Set.add (Elt.create formula) t.steps }

let peek { steps; _ } =
  try
    let s = Set.min_elt steps |> Elt.step in
    Some s
  with Not_found -> None


let consume t =
  let keep top = Set.remove top t.steps |> Set.add (use top) in
  let drop top = Set.remove top t.steps in
  try
    let top = Set.min_elt t.steps in
    match Elt.step top with
    | Gamma _   -> {t with steps = keep top}
    | Literal l -> {t with steps = drop top; lit = l::t.lit}
    | _         -> {t with steps = drop top}
  with Not_found -> t

let empty = { lit=[]; steps=Set.empty }

let singleton f = add f empty

let of_list lst =
  let f a b = add b a in
  List.fold_left f empty lst

let closure newlit t =
  (* TODO: Keeping literals as list is inefficient *)
  let candidate =
    let open Formula in
    match newlit with
    | Not (Predicate (p, pargs)) ->
        ( fun lit -> match lit with
        | Predicate (q, qargs)      when PredSymb.compare p q = 0 ->
            Some (pargs, qargs)
        | _ -> None )
    |      Predicate (p, pargs)  ->
        ( fun lit -> match lit with
        | Not(Predicate (q, qargs)) when PredSymb.compare p q = 0 ->
            Some (pargs, qargs)
        | _ -> None )
    | _ -> raise (Failure "tableau closure: literal expected")
  in
  let rec r = function
    | [] -> None
    | hd::tl ->
        match candidate hd with
        | None                -> r tl
        | Some (pargs, qargs) ->
            match List.map2 (fun a b -> a,b) pargs qargs
              |> Substitution.unifier with
            | None -> r tl
            | x -> x
  in
  (* TODO: think about efficient fair strategy *)
  r (shuffle t.lit)


let apply subst t =
  (* TODO: Store substitutions and apply them on peek *)
  let lit = List.map (Substitution.apply_formula subst) t.lit in
  let steps = set_map (Elt.apply subst) t.steps in
  { t with lit; steps }


