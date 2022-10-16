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
                ln : int
                }
type prog = Prog of fundef list * t

let rec fv = function
  | Unit(_) | Int(_, _) | Float(_, _) | ExtArray(_) -> S.empty
  | Neg(x, _) | FNeg(x, _) -> S.singleton x
  | Add(x, y, _) | Sub(x, y, _) | FAdd(x, y, _) | FSub(x, y, _) | FMul(x, y, _) | FDiv(x, y, _) | Get(x, y, _) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2, _)| IfLE(x, y, e1, e2, _) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2, _) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x, _) -> S.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e, _) -> S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys, _) -> S.of_list (x :: ys)
  | AppDir(_, xs, _) | Tuple(xs, _) -> S.of_list xs
  | LetTuple(xts, y, e, _) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))
  | Put(x, y, z, _) -> S.of_list [x; y; z]

let toplevel : fundef list ref = ref []

(* 下から上がってきた行番号を上へ *)
let rec g env known = function (* �����������Ѵ��롼�������� (caml2html: closure_g) *)
  | KNormal.Unit(ln) -> Unit(ln)
  | KNormal.Int(i, ln) -> Int(i, ln)
  | KNormal.Float(d, ln) -> Float(d, ln)
  | KNormal.Neg(x, ln) -> Neg(x, ln)
  | KNormal.Add(x, y, ln) -> Add(x, y, ln)
  | KNormal.Sub(x, y, ln) -> Sub(x, y, ln)
  | KNormal.FNeg(x, ln) -> FNeg(x, ln)
  | KNormal.FAdd(x, y, ln) -> FAdd(x, y, ln)
  | KNormal.FSub(x, y, ln) -> FSub(x, y, ln)
  | KNormal.FMul(x, y, ln) -> FMul(x, y, ln)
  | KNormal.FDiv(x, y, ln) -> FDiv(x, y, ln)
  | KNormal.IfEq(x, y, e1, e2, ln) -> IfEq(x, y, g env known e1, g env known e2, ln)
  | KNormal.IfLE(x, y, e1, e2, ln) -> IfLE(x, y, g env known e1, g env known e2, ln)
  | KNormal.Let((x, t), e1, e2, ln) -> Let((x, t), g env known e1, g (M.add x t env) known e2, ln)
  | KNormal.Var(x, ln) -> Var(x, ln)
  | KNormal.LetRec({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1; KNormal.ln = i }, e2, ln) -> (* �ؿ�����ξ�� (caml2html: closure_letrec) *)
      (* �ؿ����let rec x y1 ... yn = e1 in e2�ξ��ϡ�
         x�˼�ͳ�ѿ����ʤ�(closure��𤵤�direct�˸ƤӽФ���)
         �Ȳ��ꤷ��known���ɲä���e1�򥯥��������Ѵ����Ƥߤ� *)
      let toplevel_backup = !toplevel in
      let env' = M.add x t env in
      let known' = S.add x known in
      let e1' = g (M.add_list yts env') known' e1 in
      (* �����˼�ͳ�ѿ����ʤ��ä������Ѵ����e1'���ǧ���� *)
      (* ����: e1'��x���Ȥ��ѿ��Ȥ��ƽи��������closure��ɬ��!
         (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml����) *)
      let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
      let known', e1' =
        if S.is_empty zs then known', e1' else
        (* ���ܤ��ä������(toplevel����)���ᤷ�ơ������������Ѵ�����ľ�� *)
        (Format.eprintf "free variable(s) %s found in function %s@." (Id.pp_list (S.elements zs)) x;
         Format.eprintf "function %s cannot be directly applied in fact@." x;
         toplevel := toplevel_backup;
         let e1' = g (M.add_list yts env') known e1 in
         known, e1') in
      let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in (* ��ͳ�ѿ��Υꥹ�� *)
      let zts = List.map (fun z -> (z, M.find z env')) zs in (* �����Ǽ�ͳ�ѿ�z�η����������˰���env��ɬ�� *)
      toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1'; ln = i } :: !toplevel; (* �ȥåץ�٥�ؿ����ɲ� *)
      let e2' = g env' known' e2 in
      if S.mem x (fv e2') then (* x���ѿ��Ȥ���e2'�˽и����뤫 *)
        MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2', ln) (* �и����Ƥ����������ʤ� *)
      else
        (Format.eprintf "eliminating closure(s) %s@." x;
         e2') (* �и����ʤ����MakeCls���� *)
  | KNormal.App(x, ys, ln) when S.mem x known -> (* �ؿ�Ŭ�Ѥξ�� (caml2html: closure_app) *)
      Format.eprintf "directly applying %s@." x;
      AppDir(Id.L(x), ys, ln)
  | KNormal.App(f, xs, ln) -> AppCls(f, xs, ln)
  | KNormal.Tuple(xs, ln) -> Tuple(xs, ln)
  | KNormal.LetTuple(xts, y, e, ln) -> LetTuple(xts, y, g (M.add_list xts env) known e, ln)
  | KNormal.Get(x, y, ln) -> Get(x, y, ln)
  | KNormal.Put(x, y, z, ln) -> Put(x, y, z, ln)
  | KNormal.ExtArray(x, ln) -> ExtArray(Id.L(x), ln)
  | KNormal.ExtFunApp(x, ys, ln) -> AppDir(Id.L("min_caml_" ^ x), ys, ln)

let f e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog(List.rev !toplevel, e')
