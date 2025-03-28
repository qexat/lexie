include Stdlib.Char

let uppercase_latin_alphabet =
  List.of_seq (Stdlib.String.to_seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
;;

let lowercase_latin_alphabet =
  List.of_seq (Stdlib.String.to_seq "abcdefghijklmnopqrstuvwxyz")
;;

let is_alphabetical =
  fun char ->
  List.mem char uppercase_latin_alphabet || List.mem char lowercase_latin_alphabet
;;
