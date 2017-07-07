type t = int * string

let count = ref 0

let fresh name =
  count := !count + 1;
  !count, name

let to_string (i, name) =
  match name with
  | "" -> "v" ^ (string_of_int i)
  | s -> s
