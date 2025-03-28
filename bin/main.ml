let code = Cli.Hook.run ()
let () = if !Sys.interactive then () else exit code
