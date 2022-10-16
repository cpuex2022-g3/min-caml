(* 2���ڥ��ɤǤϤʤ�3���ڥ��ɤ�x86������֥��ɤ� *)

type id_or_imm = V of Id.t | C of int
type t = (* ̿����� (caml2html: sparcasm_t) *)
  | Ans of exp * int
  | Let of (Id.t * Type.t) * exp * t * int
and exp = (* ��İ�Ĥ�̿����б����뼰 (caml2html: sparcasm_exp) *)
  | Nop
  | Set of int * int
  | SetL of Id.l * int
  | Mov of Id.t * int
  | Neg of Id.t * int
  | Add of Id.t * id_or_imm * int
  | Sub of Id.t * id_or_imm * int
  | Ld of Id.t * id_or_imm * int * int
  | St of Id.t * Id.t * id_or_imm * int * int
  | FMovD of Id.t * int
  | FNegD of Id.t * int
  | FAddD of Id.t * Id.t * int
  | FSubD of Id.t * Id.t * int
  | FMulD of Id.t * Id.t * int
  | FDivD of Id.t * Id.t * int
  | LdDF of Id.t * id_or_imm * int * int
  | StDF of Id.t * Id.t * id_or_imm * int * int
  | Comment of string
  (* virtual instructions *)
  | IfEq of Id.t * id_or_imm * t * t * int
  | IfLE of Id.t * id_or_imm * t * t * int
  | IfGE of Id.t * id_or_imm * t * t * int(* �����оΤǤϤʤ��Τ�ɬ�� *)
  | IfFEq of Id.t * Id.t * t * t * int
  | IfFLE of Id.t * Id.t * t * t * int
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list * int
  | CallDir of Id.l * Id.t list * Id.t list * int
  | Save of Id.t * Id.t * int(* �쥸�����ѿ����ͤ򥹥��å��ѿ�����¸ (caml2html: sparcasm_save) *)
  | Restore of Id.t * int(* �����å��ѿ������ͤ����� (caml2html: sparcasm_restore) *)
type fundef = { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t; ln : int }
(* �ץ���������� = ��ư���������ơ��֥� + �ȥåץ�٥�ؿ� + �ᥤ��μ� (caml2html: sparcasm_prog) *)
type prog = Prog of (Id.l * float) list * fundef list * t

let fletd(x, e1, e2, ln) = Let((x, Type.Float), e1, e2, ln)
let seq(e1, e2, ln) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2, ln)

let regs = (* Array.init 16 (fun i -> Printf.sprintf "%%r%d" i) *)
  [| "%eax"; "%ebx"; "%ecx"; "%edx"; "%esi"; "%edi" |]
let fregs = Array.init 8 (fun i -> Printf.sprintf "%%xmm%d" i)
let allregs = Array.to_list regs
let allfregs = Array.to_list fregs
let reg_cl = regs.(Array.length regs - 1) (* closure address (caml2html: sparcasm_regcl) *)
(*
let reg_sw = regs.(Array.length regs - 1) (* temporary for swap *)
let reg_fsw = fregs.(Array.length fregs - 1) (* temporary for swap *)
*)
let reg_sp = "%ebp" (* stack pointer *)
let reg_hp = "min_caml_hp" (* heap pointer (caml2html: sparcasm_reghp) *)
(* let reg_ra = "%eax" (* return address *) *)
let is_reg x = (x.[0] = '%' || x = reg_hp)

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
let fv_id_or_imm = function V(x) -> [x] | _ -> []
let rec fv_exp = function
  | Nop | Set(_, _) | SetL(_, _) | Comment(_) | Restore(_, _) -> []
  | Mov(x, _) | Neg(x, _) | FMovD(x, _) | FNegD(x, _) | Save(x, _, _) -> [x]
  | Add(x, y', _) | Sub(x, y', _) | Ld(x, y', _, _) | LdDF(x, y', _, _) -> x :: fv_id_or_imm y'
  | St(x, y, z', _, _) | StDF(x, y, z', _, _) -> x :: y :: fv_id_or_imm z'
  | FAddD(x, y, _) | FSubD(x, y, _) | FMulD(x, y, _) | FDivD(x, y, _) -> [x; y]
  | IfEq(x, y', e1, e2, _) | IfLE(x, y', e1, e2, _) | IfGE(x, y', e1, e2, _) -> x :: fv_id_or_imm y' @ remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | IfFEq(x, y, e1, e2, _) | IfFLE(x, y, e1, e2, _) -> x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | CallCls(x, ys, zs, _) -> x :: ys @ zs
  | CallDir(_, ys, zs, _) -> ys @ zs
and fv = function
  | Ans(exp, _) -> fv_exp exp
  | Let((x, t), exp, e, _) ->
      fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
let fv e = remove_and_uniq S.empty (fv e)

let rec concat e1 xt e2 =
  match e1 with
  | Ans(exp, ln) -> Let(xt, exp, e2, ln)
  | Let(yt, exp, e1', ln) -> Let(yt, exp, concat e1' xt e2, ln)

let align i = (if i mod 8 = 0 then i else i + 4)
