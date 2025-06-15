let ( or ) =
  fun option fallback -> Option.value ~default:fallback option
;;

let ( let+ ) = Option.bind
let ( let* ) = Result.bind

module Char = Char
module Fun = Fun
module Int = Int
module Intp = Intp
module List = List
module Operators = Operators
module Option = Option
module Painter = Painter
module String = String
