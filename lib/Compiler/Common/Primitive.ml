open Batteries
open Custom

type t =
  | Nat of Intp.t
  | Unit

let nat = fun n -> Nat n
let unit = Unit

let equal =
  fun left right ->
  match left, right with
  | Nat ln, Nat rn -> Intp.equal ln rn
  | Unit, Unit -> true
  | Nat _, Unit | Unit, Nat _ -> false
;;

let show =
  fun (module Painter : Painter.TYPE) prim ->
  (match prim with
   | Nat n -> Intp.to_string n
   | Unit -> Printf.sprintf "()")
  |> Painter.paint_constant
;;
