(** [color] represents the painter palette. *)
type color =
  | None
  | Red
  | Yellow
  | Green
  | Cyan
  | Blue
  | Magenta
  | Black
  | White

(** [check_tty ()] checks whether the stdout & stderr are TTYs. *)
let check_tty () : bool =
  Out_channel.isatty stdout && Out_channel.isatty stderr
;;

module type CONFIG = sig
  val show_styling : [ `Never | `Always | `Auto ]
end

module type TYPE = sig
  (** The type of a painter. *)

  (** [make_intelligent_painter painter] takes a dumb [painter]
      that always paints and make it only paint according to
      a configuration and/or whether the stdout & stderr are
      TTYs. *)
  val make_intelligent_painter
    :  (string -> string)
    -> string
    -> string

  (** [paint_bold string] paints [string] in bold. *)
  val paint_bold : string -> string

  (** [paint_dim string] paints [string] in dim. *)
  val paint_dim : string -> string

  (** [paint_italic string] paints [string] in italic. *)
  val paint_italic : string -> string

  (** [paint_underlined string] paints [string] underlined. *)
  val paint_underlined : string -> string

  (** [paint_foreground ?bright color string] paints [string] in
      the [color]. *)
  val paint_foreground
    :  ?bright:bool
    -> color
    -> string
    -> string

  (** [paint_background ?bright color string] paints the
      background of [string] in the [color]. *)
  val paint_background
    :  ?bright:bool
    -> color
    -> string
    -> string
end

module Make (Config : CONFIG) : TYPE = struct
  let make_intelligent_painter =
    fun painter string ->
    match Config.show_styling with
    | `Never -> string
    | `Always -> painter string
    | `Auto -> if check_tty () then painter string else string
  ;;

  let make_ansi_painter =
    fun opener closer ->
    make_intelligent_painter (fun string ->
      Printf.sprintf "\x1b[%dm%s\x1b[%dm" opener string closer)
  ;;

  let paint_bold = make_ansi_painter 1 22
  let paint_dim = make_ansi_painter 2 22
  let paint_italic = make_ansi_painter 3 23
  let paint_underlined = make_ansi_painter 4 24

  let color_to_int = function
    | Black -> 0
    | Red -> 1
    | Green -> 2
    | Yellow -> 3
    | Blue -> 4
    | Magenta -> 5
    | Cyan -> 6
    | White -> 7
    | None -> 9
  ;;

  let paint_foreground =
    fun ?(bright = false) color ->
    let opener =
      color_to_int color + if bright then 40 else 30
    in
    make_ansi_painter opener 39
  ;;

  let paint_background =
    fun ?(bright = false) color ->
    let opener =
      color_to_int color + if bright then 100 else 90
    in
    make_ansi_painter opener 49
  ;;
end
