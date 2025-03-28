open Batteries

type ('config, 'input, 'output) hook =
  nurse:Nurse.t -> painter:(module Painter.TYPE) -> 'config -> 'input -> 'output option

let compose =
  fun left right ->
  fun ~nurse ~painter config input ->
  let+ next_input = left ~nurse ~painter config input in
  right ~nurse ~painter config next_input
;;
