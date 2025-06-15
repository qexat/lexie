open Custom

type t =
  | Illegal_application
  | Undefined_name of Name.t

let show =
  fun painter unreachable ->
  match unreachable with
  | Illegal_application -> "illegal application"
  | Undefined_name name ->
    Printf.sprintf "undefined name %s" (Name.show painter name)
;;
