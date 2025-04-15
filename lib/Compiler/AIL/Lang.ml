open Batteries.Operators
open Custom
open Common

type kind =
  | Arrow of parameter * kind
  | Sort of Sort.t
  | Task of task
  | Term of term

and term =
  | App of term * term
  | Fun of parameter * term
  | Hole
  | Primitive of Primitive.t
  | Var of Name.t

and parameter = Named of Name.t * kind

and task =
  | Done of kind
  | Future of (string * kind)

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
  | Task task -> show_task painter task
  | Term term -> show_term painter term

and show_term =
  fun painter term ->
  let module Painter = (val painter : Painter.TYPE) in
  match term with
  | App (func, arg) ->
    let func, args = uncurry_application func in
    Printf.sprintf
      "%s %s"
      (show_term_considering_precedence painter func ~parent:term)
      (args ++ [ arg ]
       |> List.map (show_term_considering_precedence painter ~parent:term)
       |> String.concat " ")
  | Fun (param, ret) ->
    let params, ret = uncurry_function ret in
    Printf.sprintf
      "%s %s -> %s"
      (Painter.paint_keyword "fun")
      (param :: params |> List.map (show_parameter painter) |> String.concat " ")
      (show_term_considering_precedence painter ret ~parent:term)
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
  fun painter parameter ->
  match parameter with
  | Named (name, kind) ->
    Printf.sprintf "(%s : %s)" (Name.show painter name) (show_kind painter kind)

and show_task =
  fun painter (task : task) ->
  let module Painter = (val painter : Painter.TYPE) in
  match task with
  | Done term ->
    Printf.sprintf "(%s %s)" (Painter.paint_keyword "done") (show_kind painter term)
  | Future (syntactic, kind) ->
    Printf.sprintf
      "(%s '%s' of kind %s)"
      (Painter.paint_keyword "future")
      (String.escaped syntactic)
      (show_kind painter kind)

and uncurry_function =
  fun term ->
  match term with
  | Fun (param, ret) ->
    let rest, inner_ret = uncurry_function ret in
    param :: rest, inner_ret
  | _ -> [], term

and uncurry_application =
  fun func ->
  match func with
  | App (func', last_arg) ->
    let inner_func, args = uncurry_application func' in
    inner_func, args ++ [ last_arg ]
  | _ -> func, []
;;
