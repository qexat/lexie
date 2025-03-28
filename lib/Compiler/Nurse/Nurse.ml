open Custom

type 'a renderer = (module Painter.TYPE) -> 'a -> string

type t =
  { mutable diagnostics : Diagnostic.t list
  ; mutable render_diagnosis : Diagnostic.Diagnosis.t renderer
  }

let create () = { diagnostics = []; render_diagnosis = (fun _ _ -> "<diagnosis>") }

let add_diagnostic =
  fun diagnostic nurse -> nurse.diagnostics <- diagnostic :: nurse.diagnostics
;;

let add_error = fun diagnosis -> add_diagnostic { category = Error; diagnosis }
let add_warning = fun diagnosis -> add_diagnostic { category = Warning; diagnosis }
let add_info = fun diagnosis -> add_diagnostic { category = Info; diagnosis }
let set_diagnosis_renderer = fun renderer nurse -> nurse.render_diagnosis <- renderer

let turn_warnings_into_errors =
  fun nurse ->
  let turn_warning_into_error =
    let open Diagnostic in
    fun diagnostic ->
      { diagnostic with
        category =
          (match diagnostic.category with
           | Warning -> Error
           | category -> category)
      }
  in
  nurse.diagnostics <- List.map turn_warning_into_error nurse.diagnostics
;;

let render_category =
  fun painter category ->
  let open Diagnostic in
  let module Painter = (val painter : Painter.TYPE) in
  (match (category : Category.t) with
   | Error -> Painter.paint_error
   | Warning -> Painter.paint_warning
   | Info -> Painter.paint_info)
    (Category.name category ^ ":")
;;

let render_diagnostic =
  fun diagnosis_renderer painter diagnostic ->
  match (diagnostic : Diagnostic.t) with
  | { category; diagnosis } ->
    Printf.sprintf
      "%s %s"
      (render_category painter category)
      (diagnosis_renderer painter diagnosis)
;;

let report =
  fun ?(out = stderr) painter nurse ->
  List.map (render_diagnostic nurse.render_diagnosis painter) nurse.diagnostics
  |> String.concat "\n"
  |> Printf.fprintf out "%s\n"
;;
