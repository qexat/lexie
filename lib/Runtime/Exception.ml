type kind = Incomplete_program
type t = { kind : kind }

let show =
  fun _ exn ->
  match exn.kind with
  | Incomplete_program ->
    "this program is incomplete (hole found)"
;;
