open Batteries

type t =
  { config : Config.t
  ; mutable diagnostics : Diagnostic.t list
  }

let create = fun config -> { config; diagnostics = [] }

let add_diagnostic =
  fun diagnostic doctor -> doctor.diagnostics <- diagnostic :: doctor.diagnostics
;;

let add_error = fun diagnosis -> add_diagnostic { category = Error; diagnosis }
let add_warning = fun diagnosis -> add_diagnostic { category = Warning; diagnosis }
let add_info = fun diagnosis -> add_diagnostic { category = Info; diagnosis }

type decision =
  | Pass
  | Abort

type review =
  { decision : decision
  ; details : string option
  }

let make_decision =
  fun ~strict diagnostics ->
  if
    List.exists Diagnostic.is_error diagnostics
    || (strict && List.exists Diagnostic.is_warning diagnostics)
  then Abort
  else Pass
;;

let review =
  fun painter doctor ->
  let decision = make_decision ~strict:doctor.config.strict doctor.diagnostics in
  let details =
    doctor.diagnostics
    |> List.map_on_cons (Diagnostic.show painter)
    |> Option.map (String.concat "\n")
  in
  { decision; details }
;;
