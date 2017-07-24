module VSet = Set.Make(Symbol)

type t = VSet.t

let empty    = VSet.empty
let add      = VSet.add
let union    = VSet.union
let elements = VSet.elements
let mem      = VSet.mem
let remove   = VSet.remove

