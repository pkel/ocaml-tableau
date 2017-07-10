open Term
open Formula
open Step

type state =
  | Working
  | DeadEnd
  | Aborted
  | Closed

type t =
  { closed : Branch.t list
  ; stack  : Branch.t list
  ; power  : int
  ; state  : state
  }

let state { state; _ } = state

let step t =
  match t.stack with
  | [] -> { t with state = Closed }
  | hd::tl ->
      let open Branch in
      match state hd with
      | Closed -> { t with closed = hd::t.closed ; stack = tl }
      | Open   ->
          match peek hd with
          (* no steps left, reached dead end *)
          | None -> { t with state = DeadEnd }
          | Some step ->
              match step with
              | Alpha(a1,a2) ->
                  (* expand branch, delete source formula *)
                  let b = consume hd |> add a1 |> add a2 in
                  { t with stack = b::tl }
              | Beta(b1,b2) ->
                  (* split branch, delete source formula *)
                  let l = consume hd |> add b1 in
                  let r = consume hd |> add b2 in
                  { t with stack = l::r::tl }
              | Gamma(x, f) -> (
                  match t.power with
                  | 0 -> { t with state = Aborted }
                  | _ ->
                    (* fresh variable v, x->v, keep gamma *)
                    let fresh = Variable (VarSymb.fresh "") in
                    let b = consume hd |> add (instance x fresh f) in
                    { t with stack = b::tl; power = t.power - 1 }
              )
              | Delta(x, f) ->
                  (* fresh skolem function sk, x->sk, drop delta *)
                  let args = free_vars f |> VarSet.remove x |>
                    VarSet.elements |> List.map (fun x -> Variable x)
                  in
                  let n = List.length args in
                  let sk = Function (FunSymb.fresh n "", args) in
                  let b = consume hd |> add (instance x sk f) in
                  { t with stack = b::tl }
              | DoubleNeg a
              | Literal   a ->
                  let b = consume hd |> add a in
                  { t with stack = b::tl }

let rec expand t =
  match t.state with
  | Working -> step t |> expand
  | _ -> t

let init power formulas =
  { state = Working
  ; closed = []
  ; stack = [Branch.of_list formulas]
  ; power = power
  }
