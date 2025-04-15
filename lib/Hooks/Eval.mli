open Clinic
open Custom

(** [execute doctor painter config program] executes the hook. *)
val execute
  :  doctor:Doctor.t
  -> painter:(module Painter.TYPE)
  -> Config.t
  -> AIL.Program.t
  -> unit option
