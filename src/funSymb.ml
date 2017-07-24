type t = int * int * string

let count = ref 0

let to_string (i, _, name) =
  match name with
  | "" -> "f" ^ (string_of_int i)
  | s -> s

let fresh arity name =
  count := !count + 1;
  let t = !count, arity, name in
  (* print_endline ("fresh function: " ^ (to_string t)); *)
  t

let arity (_, n, _) = n

let compare (i, _, _) (j, _, _) =
  Pervasives.compare i j
