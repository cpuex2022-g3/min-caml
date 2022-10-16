open KNormal

let find x env = try M.find x env with Not_found -> x (* �ִ��Τ���δؿ� (caml2html: beta_find) *)

(* 下から上がってきた行番号を上へ *)
let rec g env = function (* �´���롼�������� (caml2html: beta_g) *)
  | Unit(ln) -> Unit(ln)
  | Int(i, ln) -> Int(i, ln)
  | Float(d, ln) -> Float(d, ln)
  | Neg(x, ln) -> Neg(find x env, ln)
  | Add(x, y, ln) -> Add(find x env, find y env, ln)
  | Sub(x, y, ln) -> Sub(find x env, find y env, ln)
  | FNeg(x, ln) -> FNeg(find x env, ln)
  | FAdd(x, y, ln) -> FAdd(find x env, find y env, ln)
  | FSub(x, y, ln) -> FSub(find x env, find y env, ln)
  | FMul(x, y, ln) -> FMul(find x env, find y env, ln)
  | FDiv(x, y, ln) -> FDiv(find x env, find y env, ln)
  | IfEq(x, y, e1, e2, ln) -> IfEq(find x env, find y env, g env e1, g env e2, ln)
  | IfLE(x, y, e1, e2, ln) -> IfLE(find x env, find y env, g env e1, g env e2, ln)
  | Let((x, t), e1, e2, ln) -> (* let�Φ´��� (caml2html: beta_let) *)
      (match g env e1 with
      | Var(y, _) ->
          Format.eprintf "beta-reducing %s = %s@." x y;
          g (M.add x y env) e2
      | e1' ->
          let e2' = g env e2 in
          Let((x, t), e1', e2', ln))
  | LetRec({ name = xt; args = yts; body = e1; ln = i }, e2, ln) ->
      LetRec({ name = xt; args = yts; body = g env e1; ln = i }, g env e2, ln)
  | Var(x, ln) -> Var(find x env, ln) (* �ѿ����ִ� (caml2html: beta_var) *)
  | Tuple(xs, ln) -> Tuple(List.map (fun x -> find x env) xs, ln)
  | LetTuple(xts, y, e, ln) -> LetTuple(xts, find y env, g env e, ln)
  | Get(x, y, ln) -> Get(find x env, find y env, ln)
  | Put(x, y, z, ln) -> Put(find x env, find y env, find z env, ln)
  | App(g, xs, ln) -> App(find g env, List.map (fun x -> find x env) xs, ln)
  | ExtArray(x, ln) -> ExtArray(x, ln)
  | ExtFunApp(x, ys, ln) -> ExtFunApp(x, List.map (fun y -> find y env) ys, ln)

let f = g M.empty
