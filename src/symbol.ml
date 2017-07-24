type t = int * string

let count = ref 0

let to_string (i, name) =
  match name with
  | "" -> "f" ^ (string_of_int i)
  | s -> s

let fresh name =
  count := !count + 1;
  let t = !count, name in
  t

let compare (i, _) (j, _) =
  Pervasives.compare i j
