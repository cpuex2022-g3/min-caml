open KNormal

(* ����饤��Ÿ������ؿ��κ��祵���� (caml2html: inline_threshold) *)
let threshold = ref 0 (* Main��-inline���ץ����ˤ�ꥻ�åȤ���� *)

let rec size = function
  | IfEq(_, _, e1, e2, _) | IfLE(_, _, e1, e2, _)
  | Let(_, e1, e2, _) | LetRec({ body = e1 }, e2, _) -> 1 + size e1 + size e2
  | LetTuple(_, _, e, _) -> 1 + size e
  | _ -> 1

(* 下から上がってきた行番号を上へ *)
let rec g env = function (* ����饤��Ÿ���롼�������� (caml2html: inline_g) *)
  | IfEq(x, y, e1, e2, ln) -> IfEq(x, y, g env e1, g env e2, ln)
  | IfLE(x, y, e1, e2, ln) -> IfLE(x, y, g env e1, g env e2, ln)
  | Let(xt, e1, e2, ln) -> Let(xt, g env e1, g env e2, ln)
  | LetRec({ name = (x, t); args = yts; body = e1; ln = i }, e2, ln) -> (* �ؿ�����ξ�� (caml2html: inline_letrec) *)
      let env = if size e1 > !threshold then env else M.add x (yts, e1) env in
      LetRec({ name = (x, t); args = yts; body = g env e1; ln = i}, g env e2, ln)
  | App(x, ys, ln) when M.mem x env -> (* �ؿ�Ŭ�Ѥξ�� (caml2html: inline_app) *)
      let (zs, e) = M.find x env in
      Format.eprintf "inlining %s@." x;
      let env' =
        List.fold_left2
          (fun env' (z, t) y -> M.add z y env')
          M.empty
          zs
          ys in
      Alpha.g env' e
  | LetTuple(xts, y, e, ln) -> LetTuple(xts, y, g env e, ln)
  | e -> e

let f e = g M.empty e
