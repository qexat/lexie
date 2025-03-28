include module type of Stdlib.Int

module Notation : sig
  val ( = ) : t -> t -> bool
  val ( != ) : t -> t -> bool
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( % ) : t -> t -> t
end
