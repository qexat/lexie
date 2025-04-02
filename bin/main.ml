open Cli.Main

let () = if !Sys.interactive then () else exit (main ())
