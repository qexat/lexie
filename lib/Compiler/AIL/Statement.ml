open Custom

type nonrec t =
  | Let of Name.t * Kind.t option * Term.t
  | Print of Term.t

let let' = fun name ?annotation term -> Let (name, annotation, term)
let print = fun term -> Print term

let show =
  fun painter stmt ->
  let module Painter = (val painter : Painter.TYPE) in
  match stmt with
  | Let (name, annotation, body) ->
    let buffer = Buffer.create 64 in
    Buffer.add_string
      buffer
      (Printf.sprintf "%s %s" (Painter.paint_keyword "let") (Name.show painter name));
    (match annotation with
     | None -> ()
     | Some kind ->
       Buffer.add_string
         buffer
         (Printf.sprintf " %s %s" (Painter.paint_keyword ":") (Kind.show painter kind)));
    Buffer.add_string
      buffer
      (Printf.sprintf " %s %s" (Painter.paint_bold "=") (Term.show painter body));
    Buffer.contents buffer
  | Print term ->
    Printf.sprintf "%s %s" (Painter.paint_keyword "print") (Term.show painter term)
;;
