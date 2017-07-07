type t = int * int * string

let count = ref 0

let fresh arity name =
  count := !count + 1;
  !count, arity, name

let arity (_, n, _) = n

let to_string (i, _, name) =
  match name with
  | "" -> "P" ^ (string_of_int i)
  | s -> s