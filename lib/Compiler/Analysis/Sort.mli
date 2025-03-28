type t =
  | Prop
  | Type

val show : (module Custom.Painter.TYPE) -> t -> string
