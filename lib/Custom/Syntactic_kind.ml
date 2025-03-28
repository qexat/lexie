type t =
  | Atom
  | Grouping
  | App
  | Binary
  | Fun

let compare =
  fun left right ->
  match left, right with
  | Atom, Atom | Grouping, Grouping | App, App | Binary, Binary | Fun, Fun -> 0
  | Atom, (Grouping | App | Binary | Fun)
  | Grouping, (App | Binary | Fun)
  | App, (Binary | Fun)
  | Binary, Fun -> -1
  | (Grouping | App | Binary | Fun), Atom
  | (App | Binary | Fun), Grouping
  | (Binary | Fun), App
  | Fun, Binary -> 1
;;

let equal = fun left right -> compare left right = 0
let binds_tighter = fun left ~than:right -> compare left right < 0
