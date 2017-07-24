let option_filter_rev f =
  let ff acc x =
    match f x with
    | Some el -> el :: acc
    | None -> acc
  in
  List.fold_left ff []

let option_filter f lst =
  List.rev (option_filter_rev f lst)

let rec option_first f = function
  | [] -> None
  | hd :: tl ->
      match f hd with
      | None -> option_first f tl
      | x -> x
