type t = Lang.task

let return term = Lang.Done term
let future syntax kind = Lang.Future (syntax, kind)
let show = Lang.show_task
