(* exception Error of Syntax.t * Type.t * Type.t *)
exception Error of string * Type.t * Type.t
val extenv : Type.t M.t ref
val f : Syntax.t -> Syntax.t
