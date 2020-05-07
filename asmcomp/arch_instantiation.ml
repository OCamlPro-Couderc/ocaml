(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Emitaux = Emitaux(Arch_specific.Arch)

module Reload = Arch_specific.Reload.Make(Reloadgen)

module Selector_arg = struct

  module Arch = Arch_specific.Arch

  module Effect = Selectgen.Effect

  module Coeffect = Selectgen.Coeffect

  module Effect_and_coeffect = Selectgen.Effect_and_coeffect

  type environment = Selectgen.environment

  class virtual selector_generic = Spacetime_profiling.instruction_selection

end

module Selection = Arch_specific.Selection.Make(Selector_arg)

module Scheduling = Arch_specific.Scheduling.Make(Schedgen)
