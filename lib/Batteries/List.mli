include module type of Stdlib.List

val map_on_cons
  : 'item1 'item2.
  ('item1 -> 'item2) -> 'item1 list -> 'item2 list option

module Notation : sig
  val ( ++ ) : 'item. 'item t -> 'item t -> 'item t
end
