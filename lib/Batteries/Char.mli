include module type of Stdlib.Char

(** Letters of the latin alphabet in uppercase. *)
val uppercase_latin_alphabet : t list

(** Letters of the latin alphabet in lowercase. *)
val lowercase_latin_alphabet : t list

(** [is_alphabetical char] determines whether [char] is a letter
    of the latin alphabet. *)
val is_alphabetical : t -> bool
