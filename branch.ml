(* assign each step an initial precedence *)
let precedence formula =
  let open Step in
  match step(formula) with
  | Literal   _ -> 0
  | DoubleNeg _ -> 1
  | Alpha     _ -> 2
  | Beta      _ -> 3
  | Delta     _ -> 4
  | Gamma     _ -> 5

(* what happens to precedence on usage? *)
let use (p, id, formula) = (p + 10, id, formula)

(* formula on branch *)
module F =
  struct
    (* precedence * id * formula itself *)
    type t = int * int * Formula.t

    (* mutable counter to index formulas *)
    let count = ref 0

    (* new working formula increases counter *)
    let create formula =
      count := !count + 1;
      precedence(formula), !count, formula

    (* min_elt on set will gives lowest precedence and lowest id *)
    let compare (p1,i1,_) (p2,i2,_) =
      match Pervasives.compare p1 p2 with
      | 0 -> Pervasives.compare i1 i2
      | x -> x
  end

module S = Set.Make(F)

type state =
  | Closed
  | Open

(* literals and formulas seperated *)
type t =
  { lit  : Formula.t list
  ; fset : S.t
  ; state: state }


let closes newlit literals =
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
    | [] -> false
    | hd::tl ->
        match candidate hd with
        | None                -> r tl
        | Some (pargs, qargs) ->
            (List.map2 (fun a b -> a,b) pargs qargs |> Term.unifiable) || r tl
  in
  r literals

let add formula t =
  let open Step in
  match step(formula) with
  | Literal l -> ( match closes l t.lit with
      | true  -> { t with lit = l::t.lit; state = Closed }
      | false -> { t with lit = l::t.lit } )
  | _ ->
      let f = F.create(formula) in
      { t with fset = S.add f t.fset }


let state { state; _ } = state

let literals { lit; _ } = lit

let peek { fset; _ } =
  try
    let _, _, f = S.min_elt fset in
    Some f
  with Not_found -> None


let consume t =
  let keep top = S.remove top t.fset |> S.add (use top) in
  let drop top = S.remove top t.fset in
  try
    let (p, id, f) = S.min_elt t.fset in
    let open Step in
    match step(f) with
    | Gamma _ -> {t with fset = keep (p, id, f)}
    | _       -> {t with fset = drop (p, id, f)}
  with Not_found -> t

let empty = { lit=[]; fset=S.empty; state=Open }

let singleton f = add f empty

let of_list lst =
  let f a b = add b a in
  List.fold_left f empty lst

