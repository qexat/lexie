open Cmdliner

let make_term_with_config =
  fun func ->
  let open Term.Syntax in
  let+ strict =
    Arg.(value & flag & info [ "strict" ] ~doc:"Set type-checking strict mode")
  and+ print_program =
    Arg.(
      value
      & flag
      & info [ "print-program" ] ~doc:"Print the program that is being processed")
  and+ use_compiler_intrinsics =
    Arg.(
      value
      & flag
      & info
          [ "use-compiler-intrinsics" ]
          ~doc:"Use the compiler intrinsics when checking")
  and+ show_styling =
    Arg.(
      value
      & opt (enum [ "never", `Never; "always", `Always; "auto", `Auto ]) `Auto
      & info [ "show-styling" ] ~doc:"Print ANSI escape sequences")
  in
  let config : Config.t =
    { strict; print_program; use_compiler_intrinsics; show_styling }
  in
  func config
;;

let check_subcommand = Cmd.v (Cmd.info "check") (make_term_with_config Check.execute)
let run_subcommand = Cmd.v (Cmd.info "run") (make_term_with_config Run.execute)

let lexie_command =
  let doc = "TODO" in
  Cmd.group
    (Cmd.info "lexie" ~version:"<insert version here>" ~doc)
    [ check_subcommand; run_subcommand ]
;;

let main () = Cmd.eval' lexie_command
