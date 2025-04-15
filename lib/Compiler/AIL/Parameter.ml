type t = Lang.parameter

let name = function
  | Lang.Named (name, _) -> name
;;

let kind = function
  | Lang.Named (_, kind) -> kind
;;

let show = Lang.show_parameter
