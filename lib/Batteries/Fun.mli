include module type of Stdlib.Fun

module Notation : sig
  val ( <* ) : 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val ( *> ) : 'a 'b 'c. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end
