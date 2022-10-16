open KNormal

let memi x env =
  try (match M.find x env with Int(_) -> true | _ -> false)
  with Not_found -> false
let memf x env =
  try (match M.find x env with Float(_) -> true | _ -> false)
  with Not_found -> false
let memt x env =
  try (match M.find x env with Tuple(_) -> true | _ -> false)
  with Not_found -> false

let findi x env = (match M.find x env with Int(i, _) -> i | _ -> raise Not_found)
let findf x env = (match M.find x env with Float(d, _) -> d | _ -> raise Not_found)
let findt x env = (match M.find x env with Tuple(ys, _) -> ys | _ -> raise Not_found)

(* 下から上がってきた行番号を上へ *)
let rec g env = function (* ������߹��ߥ롼�������� (caml2html: constfold_g) *)
  | Var(x, ln) when memi x env -> Int((findi x env), ln)
  (* | Var(x) when memf x env -> Float(findf x env) *)
  (* | Var(x) when memt x env -> Tuple(findt x env) *)
  | Neg(x, ln) when memi x env -> Int((-(findi x env)), ln)
  | Add(x, y, ln) when memi x env && memi y env -> Int((findi x env + findi y env), ln) (* ­�����Υ����� (caml2html: constfold_add) *)
  | Sub(x, y, ln) when memi x env && memi y env -> Int((findi x env - findi y env), ln)
  | FNeg(x, ln) when memf x env -> Float((-.(findf x env)), ln)
  | FAdd(x, y, ln) when memf x env && memf y env -> Float((findf x env +. findf y env), ln)
  | FSub(x, y, ln) when memf x env && memf y env -> Float((findf x env -. findf y env), ln)
  | FMul(x, y, ln) when memf x env && memf y env -> Float((findf x env *. findf y env), ln)
  | FDiv(x, y, ln) when memf x env && memf y env -> Float((findf x env /. findf y env), ln)
  | IfEq(x, y, e1, e2, _) when memi x env && memi y env -> if findi x env = findi y env then g env e1 else g env e2
  | IfEq(x, y, e1, e2, _) when memf x env && memf y env -> if findf x env = findf y env then g env e1 else g env e2
  | IfEq(x, y, e1, e2, ln) -> IfEq(x, y, g env e1, g env e2, ln)
  | IfLE(x, y, e1, e2, _) when memi x env && memi y env -> if findi x env <= findi y env then g env e1 else g env e2
  | IfLE(x, y, e1, e2, _) when memf x env && memf y env -> if findf x env <= findf y env then g env e1 else g env e2
  | IfLE(x, y, e1, e2, ln) -> IfLE(x, y, g env e1, g env e2, ln)
  | Let((x, t), e1, e2, ln) -> (* let�Υ����� (caml2html: constfold_let) *)
      let e1' = g env e1 in
      let e2' = g (M.add x e1' env) e2 in
      Let((x, t), e1', e2', ln)
  | LetRec({ name = x; args = ys; body = e1; ln = i }, e2, ln) ->
      LetRec({ name = x; args = ys; body = g env e1; ln = i }, g env e2, ln)
  | LetTuple(xts, y, e, ln) when memt y env ->
      List.fold_left2
        (fun e' xt z -> Let(xt, Var(z, ln), e', ln))
        (g env e)
        xts
        (findt y env)
  | LetTuple(xts, y, e, ln) -> LetTuple(xts, y, g env e, ln)
  | e -> e

let f = g M.empty
