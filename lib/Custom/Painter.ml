open Batteries
open Operators
include Painter

module type TYPE = sig
  include TYPE

  val paint_constant : string -> string
  val paint_quoted : string -> string
  val paint_type : string -> string
  val paint_function : string -> string
  val paint_keyword : string -> string
  val paint_name : string -> string
  val paint_hole : string -> string
  val paint_error : string -> string
  val paint_warning : string -> string
  val paint_info : string -> string
  val paint_note : string -> string
end

module Make (Config : CONFIG) : TYPE = struct
  include Make (Config)

  let paint_constant = paint_foreground Red
  let paint_quoted = paint_foreground Green
  let paint_type = paint_foreground Yellow
  let paint_function = paint_foreground Blue
  let paint_keyword = paint_foreground Magenta *> paint_bold
  let paint_name = paint_foreground Cyan
  let paint_hole = paint_background ~bright:true Red
  let paint_error = paint_foreground Red *> paint_bold
  let paint_warning = paint_foreground Yellow *> paint_bold
  let paint_info = paint_foreground Cyan *> paint_bold
  let paint_note = paint_foreground Magenta *> paint_bold
end
