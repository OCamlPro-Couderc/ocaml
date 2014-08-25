in namespace Functor
with Functor.(Intf)

(* It has the type of Intf as one can notice, it is required since we cannot use
   the type of an alias (which is the case for Intf) as a functor argument
   See issue 6307 @ Mantis bug tracker *)
module type I =
  sig
    type t
    val create: int -> t
  end

module Make =
  functor (I: I) ->
    struct
      type x = I.t
      let mk = I.create
    end
