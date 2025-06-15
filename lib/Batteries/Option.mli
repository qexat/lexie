include module type of Stdlib.Option

(** [on_none func option] calls [func] if [option] is [None] and
    returns [option] unchanged. *)
val on_none
  : 'item.
  (unit -> unit) -> 'item option -> 'item option
