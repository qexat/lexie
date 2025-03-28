open Batteries

type t = Statement.t list

let show =
  fun painter program ->
  program |> List.map (Statement.show painter) |> String.concat "\n"
;;
