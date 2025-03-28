open Custom

type t =
  | Error
  | Warning
  | Info

let name = function
  | Error -> "error"
  | Warning -> "warning"
  | Info -> "info"
;;

let get_painter_function =
  fun (module Painter : Painter.TYPE) -> function
  | Error -> Painter.paint_error
  | Warning -> Painter.paint_warning
  | Info -> Painter.paint_info
;;

let show = fun painter category -> get_painter_function painter category (name category)
