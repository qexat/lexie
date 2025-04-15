type t = Lang.task

let return kind = Lang.Done kind
let future syntax kind = Lang.Future (syntax, kind)
let show = Lang.show_task
