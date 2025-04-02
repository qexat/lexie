open Cmdliner

let check_subcommand =
  let term =
    let open Term.Syntax in
    let+ check = Term.const Hook.check
    and+ strict =
      Arg.(value & flag & info [ "strict" ] ~doc:"Set type-checking strict mode")
    and+ print_program =
      Arg.(
        value
        & flag
        & info [ "print-program" ] ~doc:"Print the program that is being checked")
    and+ use_compiler_intrinsics =
      Arg.(
        value
        & flag
        & info
            [ "use-compiler-intrinsics" ]
            ~doc:"Use the compiler intrinsics when checking")
    in
    check strict print_program use_compiler_intrinsics ()
  in
  Cmd.v (Cmd.info "check") term
;;

let run_subcommand =
  let term =
    let open Term.Syntax in
    let+ run = Term.const Hook.run
    and+ strict =
      Arg.(value & flag & info [ "strict" ] ~doc:"Set type-checking strict mode")
    and+ print_program =
      Arg.(
        value & flag & info [ "print-program" ] ~doc:"Print the program that is being run")
    and+ use_compiler_intrinsics =
      Arg.(
        value
        & flag
        & info
            [ "use-compiler-intrinsics" ]
            ~doc:"Use the compiler intrinsics when checking and running")
    in
    run strict print_program use_compiler_intrinsics ()
  in
  Cmd.v (Cmd.info "run") term
;;

let lexie_command =
  let doc = "TODO" in
  Cmd.group (Cmd.info "lexie" ~version:"%%VERSION%%" ~doc) [ check_subcommand ]
;;

let main () = Cmd.eval' lexie_command
