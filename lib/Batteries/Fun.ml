include Stdlib.Fun

module Notation = struct
  let ( <* ) = compose
  let ( *> ) = fun f g -> compose g f
end
