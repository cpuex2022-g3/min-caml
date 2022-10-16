open KNormal

let rec effect = function (* �����Ѥ�̵ͭ (caml2html: elim_effect) *)
  | Let(_, e1, e2, _) | IfEq(_, _, e1, e2, _) | IfLE(_, _, e1, e2, _) -> effect e1 || effect e2
  | LetRec(_, e, _) | LetTuple(_, _, e, _) -> effect e
  | App _ | Put _ | ExtFunApp _ -> true
  | _ -> false

(* 下から上がってきた行番号を上へ *)
let rec f = function (* �����������롼�������� (caml2html: elim_f) *)
  | IfEq(x, y, e1, e2, ln) -> IfEq(x, y, f e1, f e2, ln)
  | IfLE(x, y, e1, e2, ln) -> IfLE(x, y, f e1, f e2, ln)
  | Let((x, t), e1, e2, ln) -> (* let�ξ�� (caml2html: elim_let) *)
      let e1' = f e1 in
      let e2' = f e2 in
      if effect e1' || S.mem x (fv e2') then Let((x, t), e1', e2', ln) else
      (Format.eprintf "eliminating variable %s@." x;
       e2')
  | LetRec({ name = (x, t); args = yts; body = e1; ln = i }, e2, ln) -> (* let rec�ξ�� (caml2html: elim_letrec) *)
      let e2' = f e2 in
      if S.mem x (fv e2') then
        LetRec({ name = (x, t); args = yts; body = f e1; ln = i }, e2', ln)
      else
        (Format.eprintf "eliminating function %s@." x;
         e2')
  | LetTuple(xts, y, e, ln) ->
      let xs = List.map fst xts in
      let e' = f e in
      let live = fv e' in
      if List.exists (fun x -> S.mem x live) xs then LetTuple(xts, y, e', ln) else
      (Format.eprintf "eliminating variables %s@." (Id.pp_list xs);
       e')
  | e -> e
