open Custom
open Clinic

(** [exec doctor painter config program] executes the hook. *)
val exec
  :  doctor:Doctor.t
  -> painter:(module Painter.TYPE)
  -> Config.t
  -> Tail.Program.t
  -> Tail.Program.t option
