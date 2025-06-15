open Custom

type t =
  { category : Category.t
  ; diagnosis : Diagnosis.t
  }

let is_error =
  fun diagnostic ->
  match diagnostic.category with
  | Error -> true
  | _ -> false
;;

let is_warning =
  fun diagnostic ->
  match diagnostic.category with
  | Warning -> true
  | _ -> false
;;

let show =
  fun painter diagnostic ->
  let module Painter = (val painter : Painter.TYPE) in
  Printf.sprintf
    "%s%s %s"
    (Category.show painter diagnostic.category)
    (Category.get_painter_function
       painter
       diagnostic.category
       ":")
    (Diagnosis.show painter diagnostic.diagnosis)
;;
