type t = Lang.kind

let arrow = fun param ret -> Lang.Arrow (param, ret)
let sort = fun sort -> Lang.Sort sort
let term = fun term -> Lang.Term term
let show = Lang.show_kind
