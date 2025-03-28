open Batteries.Operators
open Custom
open Common

type kind =
  | Arrow of parameter * kind
  | Sort of Sort.t
  | Term of term

and term =
  | App of term * term
  | Fun of parameter * term
  | Hole
  | Primitive of Primitive.t
  | Var of Name.t

and parameter = Name.t * kind

let term_to_syntactic_kind = function
  | App _ -> Syntactic_kind.App
  | Fun _ -> Syntactic_kind.Fun
  | Hole | Primitive _ | Var _ -> Syntactic_kind.Atom
;;

let rec show_kind =
  fun painter kind ->
  match kind with
  | Arrow (param, ret) ->
    Printf.sprintf "%s -> %s" (show_parameter painter param) (show_kind painter ret)
  | Sort sort -> Sort.show painter sort
  | Term term -> show_term painter term

and show_term =
  fun painter term ->
  let module Painter = (val painter : Painter.TYPE) in
  match term with
  | App (func, arg) ->
    Printf.sprintf
      "%s %s"
      (show_term_considering_precedence painter func ~parent:term)
      (show_term_considering_precedence painter arg ~parent:term)
  | Fun (param, body) ->
    Printf.sprintf
      "%s %s -> %s"
      (Painter.paint_keyword "fun")
      (show_parameter painter param)
      (show_term_considering_precedence painter body ~parent:term)
  | Hole -> Painter.paint_hole "_"
  | Primitive prim -> Primitive.show (module Painter) prim
  | Var name -> Name.show (module Painter) name

and show_term_considering_precedence =
  fun painter term ~parent ->
  let module Painter = (val painter : Painter.TYPE) in
  let repr = show_term (module Painter) term in
  if
    Syntactic_kind.binds_tighter
      (term_to_syntactic_kind parent)
      ~than:(term_to_syntactic_kind term)
  then "(" <> repr <> ")"
  else repr

and show_parameter =
  fun painter (name, kind) ->
  Printf.sprintf "(%s : %s)" (Name.show painter name) (show_kind painter kind)
;;
