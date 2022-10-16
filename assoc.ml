(* flatten let-bindings (just for prettier printing) *)

open KNormal

(* 下から上がってきた行番号を上へ *)
let rec f = function (* �ͥ��Ȥ���let�δ��� (caml2html: assoc_f) *)
  | IfEq(x, y, e1, e2, ln) -> IfEq(x, y, f e1, f e2, ln)
  | IfLE(x, y, e1, e2, ln) -> IfLE(x, y, f e1, f e2, ln)
  | Let(xt, e1, e2, ln) -> (* let�ξ�� (caml2html: assoc_let) *)
      let rec insert = function
        | Let(yt, e3, e4, ln) -> Let(yt, e3, insert e4, ln)
        | LetRec(fundefs, e, ln) -> LetRec(fundefs, insert e, ln)
        | LetTuple(yts, z, e, ln) -> LetTuple(yts, z, insert e, ln)
        | e -> Let(xt, e, f e2, ln) in
      insert (f e1)
  | LetRec({ name = xt; args = yts; body = e1; ln = i }, e2, ln) ->
      LetRec({ name = xt; args = yts; body = f e1; ln = i }, f e2, ln)
  | LetTuple(xts, y, e, ln) -> LetTuple(xts, y, f e, ln)
  | e -> e
