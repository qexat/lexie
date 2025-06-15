open Batteries
open Clinic

type ('config, 'input, 'output) hook =
  doctor:Doctor.t
  -> painter:(module Painter.TYPE)
  -> 'config
  -> 'input
  -> 'output option

let compose =
  fun left right ->
  fun ~doctor ~painter config input ->
  let+ next_input = left ~doctor ~painter config input in
  right ~doctor ~painter config next_input
;;
