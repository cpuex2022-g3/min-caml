type t =  
  | Unit of int
  | Int of int * int
  | Float of float * int
  | Neg of Id.t * int
  | Add of Id.t * Id.t * int
  | Sub of Id.t * Id.t * int
  | FNeg of Id.t * int
  | FAdd of Id.t * Id.t * int
  | FSub of Id.t * Id.t * int
  | FMul of Id.t * Id.t * int
  | FDiv of Id.t * Id.t * int
  | IfEq of Id.t * Id.t * t * t * int(* 比較 + 分岐 (caml2html: knormal_branch) *)
  | IfLE of Id.t * Id.t * t * t * int(* 比較 + 分岐 *)
  | Let of (Id.t * Type.t) * t * t * int
  | Var of Id.t * int
  | LetRec of fundef * t * int
  | App of Id.t * Id.t list * int
  | Tuple of Id.t list * int
  | LetTuple of (Id.t * Type.t) list * Id.t * t * int
  | Get of Id.t * Id.t * int
  | Put of Id.t * Id.t * Id.t * int
  | ExtArray of Id.t * int
  | ExtFunApp of Id.t * Id.t list * int
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t; ln : int }


val fv : t -> S.t
val f : Syntax.t -> t
