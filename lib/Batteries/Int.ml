include Stdlib.Int

module Notation = struct
  let ( = ) = equal
  let ( != ) = fun i1 i2 -> not (i1 = i2)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( % ) = rem
end
