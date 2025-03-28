module type CONFIG = Check.CONFIG

let exec = Hook.compose Check.exec Eval.exec
