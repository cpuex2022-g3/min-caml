type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* �����������Ѵ���μ� (caml2html: closure_t) *)
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
  | IfEq of Id.t * Id.t * t * t * int
  | IfLE of Id.t * Id.t * t * t * int
  | Let of (Id.t * Type.t) * t * t * int
  | Var of Id.t * int
  | MakeCls of (Id.t * Type.t) * closure * t * int
  | AppCls of Id.t * Id.t list * int
  | AppDir of Id.l * Id.t list * int
  | Tuple of Id.t list * int
  | LetTuple of (Id.t * Type.t) list * Id.t * t * int
  | Get of Id.t * Id.t * int
  | Put of Id.t * Id.t * Id.t * int
  | ExtArray of Id.l * int
type fundef = { name : Id.l * Type.t;
                args : (Id.t * Type.t) list;
                formal_fv : (Id.t * Type.t) list;
                body : t;
                ln   : int
                }
type prog = Prog of fundef list * t

val fv : t -> S.t
val f : KNormal.t -> prog
