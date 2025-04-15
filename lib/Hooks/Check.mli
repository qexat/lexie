open Custom
open Clinic

(** [execute doctor painter config program] executes the hook. *)
val execute
  :  doctor:Doctor.t
  -> painter:(module Painter.TYPE)
  -> Config.t
  -> Ail.Program.t
  -> Ail.Program.t option
