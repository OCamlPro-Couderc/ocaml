with (Linked) of Y
and (Linked as Linked_Z) of Z;;

let _ =
  ignore @@ Linked.create 42;
  ignore @@ Linked_Z.create 24
