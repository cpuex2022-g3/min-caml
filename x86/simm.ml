open Asm

(* 下から来た行番号を上へ *)
let rec g env = function (* ̿�����¨�ͺ�Ŭ�� (caml2html: simm13_g) *)
  | Ans(exp, ln) -> Ans(g' env exp, ln)
  | Let((x, t), Set(i, ln1), e, ln2) ->
      (* Format.eprintf "found simm %s = %d@." x i; *)
      let e' = g (M.add x i env) e in
      if List.mem x (fv e') then Let((x, t), Set(i,ln1), e', ln2) else
      ((* Format.eprintf "erased redundant Set to %s@." x; *)
       e')
  | Let(xt, exp, e, ln) -> Let(xt, g' env exp, g env e, ln)
and g' env = function (* ��̿���¨�ͺ�Ŭ�� (caml2html: simm13_gprime) *)
  | Add(x, V(y), ln) when M.mem y env -> Add(x, C(M.find y env), ln)
  | Add(x, V(y), ln) when M.mem x env -> Add(y, C(M.find x env), ln)
  | Sub(x, V(y), ln) when M.mem y env -> Sub(x, C(M.find y env), ln)
  | Ld(x, V(y), i, ln) when M.mem y env -> Ld(x, C(M.find y env), i, ln)
  | St(x, y, V(z), i, ln) when M.mem z env -> St(x, y, C(M.find z env), i, ln)
  | LdDF(x, V(y), i, ln) when M.mem y env -> LdDF(x, C(M.find y env), i, ln)
  | StDF(x, y, V(z), i, ln) when M.mem z env -> StDF(x, y, C(M.find z env), i, ln)
  | IfEq(x, V(y), e1, e2, ln) when M.mem y env -> IfEq(x, C(M.find y env), g env e1, g env e2, ln)
  | IfLE(x, V(y), e1, e2, ln) when M.mem y env -> IfLE(x, C(M.find y env), g env e1, g env e2, ln)
  | IfGE(x, V(y), e1, e2, ln) when M.mem y env -> IfGE(x, C(M.find y env), g env e1, g env e2, ln)
  | IfEq(x, V(y), e1, e2, ln) when M.mem x env -> IfEq(y, C(M.find x env), g env e1, g env e2, ln)
  | IfLE(x, V(y), e1, e2, ln) when M.mem x env -> IfGE(y, C(M.find x env), g env e1, g env e2, ln)
  | IfGE(x, V(y), e1, e2, ln) when M.mem x env -> IfLE(y, C(M.find x env), g env e1, g env e2, ln)
  | IfEq(x, y', e1, e2, ln) -> IfEq(x, y', g env e1, g env e2, ln)
  | IfLE(x, y', e1, e2, ln) -> IfLE(x, y', g env e1, g env e2, ln)
  | IfGE(x, y', e1, e2, ln) -> IfGE(x, y', g env e1, g env e2, ln)
  | IfFEq(x, y, e1, e2, ln) -> IfFEq(x, y, g env e1, g env e2, ln)
  | IfFLE(x, y, e1, e2, ln) -> IfFLE(x, y, g env e1, g env e2, ln)
  | e -> e

let h { name = l; args = xs; fargs = ys; body = e; ret = t; ln = i } = (* �ȥåץ�٥�ؿ���¨�ͺ�Ŭ�� *)
  { name = l; args = xs; fargs = ys; body = g M.empty e; ret = t; ln = i }

let f (Prog(data, fundefs, e)) = (* �ץ���������Τ�¨�ͺ�Ŭ�� *)
  Prog(data, List.map h fundefs, g M.empty e)
