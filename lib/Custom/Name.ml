include Batteries.String.Constrain (struct
    open Batteries

    let predicate =
      fun string ->
      String.for_all (fun char -> Char.is_alphabetical char || Char.equal char '_') string
      && not (String.equal string "_")
    ;;
  end)

module Type = Batteries.String.Constrain (struct
    open Batteries

    let predicate =
      fun string -> predicate string && List.mem string.[0] Char.uppercase_latin_alphabet
    ;;
  end)

let is_type = fun name -> Type.predicate (to_string name)

let show =
  fun (module Painter : Painter.TYPE) name ->
  if is_type name
  then Painter.paint_type (to_string name)
  else Painter.paint_name (to_string name)
;;
