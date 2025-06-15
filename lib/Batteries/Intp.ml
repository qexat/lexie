type t = int

let zero = Int.zero
let one = Int.one
let equal = Int.equal
let compare = Int.compare
let succ = Int.succ
let pred = fun p -> if equal p 0 then 0 else Int.pred p
let add = Int.add

let sub =
  fun p1 p2 -> if compare p1 p2 < 0 then 0 else Int.sub p1 p2
;;

let mul = Int.mul
let div = Int.div
let modulo = Int.rem
let rem = Int.rem
let min = Int.min
let max = Int.max
let abs = fun p -> p
let to_int = fun p -> p
let to_float = Int.to_float
let to_string = Int.to_string
let of_int = fun i -> if i < 0 then None else Some i

let of_int_exn =
  fun i ->
  match of_int i with
  | None -> invalid_arg "of_int: integer must be non-negative"
  | Some p -> p
;;

module Notation = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = modulo
  let ( % ) = rem
  let ( ~++ ) = succ
  let ( ~-- ) = pred
end
