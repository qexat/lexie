open Custom

(* TODO: universes *)
type t =
  | Prop
  | Type

let show =
  fun (module Painter : Painter.TYPE) -> function
  | Prop -> Painter.paint_type "Prop"
  | Type -> Painter.paint_type "Type"
;;
