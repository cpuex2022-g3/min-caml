(* rename identifiers to make them unique (alpha-conversion) *)

open KNormal

let find x env = try M.find x env with Not_found -> x

(* 下から上がってきた行番号を上へ *)
let rec g env = function (* ���Ѵ��롼�������� (caml2html: alpha_g) *)
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
  | Let((x, t), e1, e2, ln) -> (* let�Φ��Ѵ� (caml2html: alpha_let) *)
      let x' = Id.genid x in
      Let((x', t), g env e1, g (M.add x x' env) e2, ln)
  | Var(x, ln) -> Var(find x env, ln)
  | LetRec({ name = (x, t); args = yts; body = e1; ln = i }, e2, ln) -> (* let rec�Φ��Ѵ� (caml2html: alpha_letrec) *)
      let env = M.add x (Id.genid x) env in
      let ys = List.map fst yts in
      let env' = M.add_list2 ys (List.map Id.genid ys) env in
      LetRec({ name = (find x env, t);
               args = List.map (fun (y, t) -> (find y env', t)) yts;
               body = g env' e1; ln = i },
             g env e2, ln)
  | App(x, ys, ln) -> App(find x env, List.map (fun y -> find y env) ys, ln)
  | Tuple(xs, ln) -> Tuple(List.map (fun x -> find x env) xs, ln)
  | LetTuple(xts, y, e, ln) -> (* LetTuple�Φ��Ѵ� (caml2html: alpha_lettuple) *)
      let xs = List.map fst xts in
      let env' = M.add_list2 xs (List.map Id.genid xs) env in
      LetTuple(List.map (fun (x, t) -> (find x env', t)) xts,
               find y env,
               g env' e, ln)
  | Get(x, y, ln) -> Get(find x env, find y env, ln)
  | Put(x, y, z, ln) -> Put(find x env, find y env, find z env, ln)
  | ExtArray(x, ln) -> ExtArray(x, ln)
  | ExtFunApp(x, ys, ln) -> ExtFunApp(x, List.map (fun y -> find y env) ys, ln)

let f = g M.empty
