include module type of Stdlib.List

module Notation : sig
  val ( ++ ) : 'item. 'item t -> 'item t -> 'item t
end
