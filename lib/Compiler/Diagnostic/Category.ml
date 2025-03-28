type t =
  | Error
  | Warning
  | Info

let name = function
  | Error -> "error"
  | Warning -> "warning"
  | Info -> "info"
;;
