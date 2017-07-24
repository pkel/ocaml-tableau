module Elt = StepOnBranch

module Set = Set.Make(Elt)

let set_map f set =
  let open Set in
  elements set |> List.map f |> of_list

(* TODO: get rid *)
let () = Random.self_init ()
(* let () = Random.init 42 *)
let shuffle d =
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond


(* literals and formulas separated *)
type t =
  { neglit : Formula.t list
  ; poslit : Formula.t list
  ; steps : Set.t
  }


let print t =
  let lit = List.concat [t.neglit; t.poslit]
    |> List.map Formula.to_string
    |> String.concat "; "
  in
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
  let keep top = Set.remove top t.steps |> Set.add (Elt.use top) in
  let drop top = Set.remove top t.steps in
  try
    let top = Set.min_elt t.steps in
    let open Step in
    match Elt.step top with
    | Gamma _   -> {t with steps = keep top}
    | Literal l ->
        (
          let open Formula in
          match l with
          | Not (f) -> {t with steps = drop top; neglit = f::t.neglit }
          |      f  -> {t with steps = drop top; poslit = f::t.poslit }
        )
    (* Strict Tableau: Don't use other stuff twice *)
    | _         -> {t with steps = drop top}
  with Not_found -> t

let empty =
  { poslit=[]
  ; neglit=[]
  ; steps=Set.empty }

let singleton f = add f empty

let of_list lst =
  let f a b = add b a in
  List.fold_left f empty lst

let closure newlit t =
  let open Formula in
  let neg, p, pargs =
    match newlit with
    | Not (Predicate (p, pargs)) -> true,  p, pargs
    |      Predicate (p, pargs)  -> false, p, pargs
    | _ -> raise (Failure "tableau closure: literal expected")
  in
  let candidates =
    let f = function
      | Predicate (q, qargs) when PredSymb.compare p q = 0 -> Some qargs
      | _ -> None
    in
    match neg with
    (* TODO: Keeping literals as list is inefficient *)
    | true  -> t.poslit |> ListX.option_filter_rev f
    | false -> t.neglit |> ListX.option_filter_rev f
  in
  let f qargs =
    List.map2 (fun a b -> a,b) pargs qargs |> Substitution.unifier
  in
  (* TODO: think about efficient fair strategy *)
  ListX.option_first f (shuffle candidates)


let apply subst t =
  (* TODO: Store substitutions and apply them on peek *)
  let poslit = List.map (Substitution.apply_formula subst) t.poslit in
  let neglit = List.map (Substitution.apply_formula subst) t.neglit in
  let steps = set_map (Elt.apply subst) t.steps in
  { t with poslit; neglit; steps }


