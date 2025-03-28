include Stdlib.List

let map_on_cons =
  fun func list ->
  match list with
  | [] -> None
  | _ -> Some (map func list)
;;

module Notation = struct
  let ( ++ ) = append
end
