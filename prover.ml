open Formula
open Step
open Branch

module C = Convenience

exception GammaBoundaryReached of int

(* find unclosable branch *)
let tableau limit formula =
  let n = ref limit in
  let count () =
    n := !n -1;
    if !n <= 0 then raise (GammaBoundaryReached limit) else ()
  in
  let rec expand = function
    (* match unexpanded branches *)
    | [] -> None
    | hd::tl ->
        match state hd with
        | Closed -> expand tl
        | Open   ->
            match peek hd with
            | None -> Some hd
            | Some formula ->
                match step(formula) with
                | Alpha(a1,a2) ->
                    (* expand branch, delete source formula *)
                    let b = consume hd |> add a1 |> add a2 in
                    expand (b::tl)
                | Beta(b1,b2) ->
                    (* split branch, delete source formula *)
                    let l = consume hd |> add b1 in
                    let r = consume hd |> add b2 in
                    expand (l::r::tl)
                | Gamma(x, f) ->
                    count () ;
                    (* fresh variable v, x->v, keep gamma *)
                    let fresh = C.fresh_var "" in
                    let b = consume hd |> add (instance x fresh f) in
                    expand (b::tl)
                | Delta(x, f) ->
                    (* fresh skolem function sk, x->sk, drop delta *)
                    let args = free_vars formula |> VarSet.elements |>
                      List.map (fun x -> Term.Variable x)
                    in
                    let n = List.length args in
                    let sk = Term.Function (FunSymb.fresh n "", args) in
                    let b = consume hd |> add (instance x sk f) in
                    expand (b::tl)
                | DoubleNeg a
                | Literal   a ->
                    let b = consume hd |> add a in
                    expand (b::tl)
  in
  expand [singleton (Not formula)]


