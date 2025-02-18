open Asm

(* for register coalescing *)
(* [XXX] Call�����ä��顢�����������̵��̣�Ȥ������ո��̤ʤΤ��ɤ�ʤ���
         ���Τ���ˡ�Call�����ä����ɤ����פ��֤��ͤ���1���Ǥ˴ޤ�롣 *)
let rec target' src (dest, t) = function
  | Mov(x, _) when x = src && is_reg dest ->
      assert (t <> Type.Unit);
      assert (t <> Type.Float);
      false, [dest]
  | FMovD(x, _) when x = src && is_reg dest ->
      assert (t = Type.Float);
      false, [dest]
  | IfEq(_, _, e1, e2, _) | IfLE(_, _, e1, e2, _) | IfGE(_, _, e1, e2, _)
  | IfFEq(_, _, e1, e2, _) | IfFLE(_, _, e1, e2, _) ->
      let c1, rs1 = target src (dest, t) e1 in
      let c2, rs2 = target src (dest, t) e2 in
      c1 && c2, rs1 @ rs2
  | CallCls(x, ys, zs, _) ->
      true, (target_args src regs 0 ys @
             target_args src fregs 0 zs @
             if x = src then [reg_cl] else [])
  | CallDir(_, ys, zs, _) ->
      true, (target_args src regs 0 ys @
             target_args src fregs 0 zs)
  | _ -> false, []
and target src dest = function (* register targeting (caml2html: regalloc_target) *)
  | Ans(exp, _) -> target' src dest exp
  | Let(xt, exp, e, _) ->
      let c1, rs1 = target' src xt exp in
      if c1 then true, rs1 else
      let c2, rs2 = target src dest e in
      c2, rs1 @ rs2
and target_args src all n = function (* auxiliary function for Call *)
  | [] -> []
  | y :: ys when src = y (* && n <= List.length all - 2 *) ->
      all.(n) :: target_args src all (n + 1) ys
  | _ :: ys -> target_args src all (n + 1) ys
(* "register sourcing" (?) as opposed to register targeting *)
(* ��x86��2���ڥ���̿��Τ����register coalescing�� *)
let rec source t = function
  | Ans(exp, _) -> source' t exp
  | Let(_, _, e, _) -> source t e
and source' t = function
  | Mov(x, _) | Neg(x, _) | Add(x, C _, _) | Sub(x, _, _) | FMovD(x, _) | FNegD(x, _) | FSubD(x, _, _) | FDivD(x, _, _) -> [x]
  | Add(x, V y, _) | FAddD(x, y, _) | FMulD(x, y, _) -> [x; y]
  | IfEq(_, _, e1, e2, _) | IfLE(_, _, e1, e2, _) | IfGE(_, _, e1, e2, _) | IfFEq(_, _, e1, e2, _) | IfFLE(_, _, e1, e2, _) ->
      source t e1 @ source t e2
  | CallCls _ | CallDir _ -> (match t with Type.Unit -> [] | Type.Float -> [fregs.(0)] | _ -> [regs.(0)])
  | _ -> []

type alloc_result = (* alloc�ˤ�����spilling�����ä����ɤ�����ɽ���ǡ����� *)
  | Alloc of Id.t (* allocated register *)
  | Spill of Id.t (* spilled variable *)
let rec alloc cont regenv x t prefer =
  (* allocate a register or spill a variable *)
  assert (not (M.mem x regenv));
  let all =
    match t with
    | Type.Unit -> [] (* dummy *)
    | Type.Float -> allfregs
    | _ -> allregs in
  if all = [] then Alloc("%unit") else (* [XX] ad hoc *)
  if is_reg x then Alloc(x) else
  let free = fv cont in
  try
    let live = (* �����Ƥ���쥸���� *)
      List.fold_left
        (fun live y ->
          if is_reg y then S.add y live else
          try S.add (M.find y regenv) live
          with Not_found -> live)
        S.empty
        free in
    let r = (* �����Ǥʤ��쥸������õ�� *)
      List.find
        (fun r -> not (S.mem r live))
        (prefer @ all) in
    (* Format.eprintf "allocated %s to %s@." x r; *)
    Alloc(r)
  with Not_found ->
    Format.eprintf "register allocation failed for %s@." x;
    let y = (* ���ι礦�쥸�����ѿ���õ�� *)
      List.find
        (fun y ->
          not (is_reg y) &&
          try List.mem (M.find y regenv) all
          with Not_found -> false)
        (List.rev free) in
    Format.eprintf "spilling %s from %s@." y (M.find y regenv);
    Spill(y)

(* auxiliary function for g and g'_and_restore *)
let add x r regenv =
  if is_reg x then (assert (x = r); regenv) else
  M.add x r regenv

(* auxiliary functions for g' *)
exception NoReg of Id.t * Type.t
let find x t regenv =
  if is_reg x then x else
  try M.find x regenv
  with Not_found -> raise (NoReg(x, t))
let find' x' regenv =
  match x' with
  | V(x) -> V(find x Type.Int regenv)
  | c -> c

let rec g ln dest cont regenv = function (* ̿����Υ쥸����������� (caml2html: regalloc_g) *)
  | Ans(exp, _) -> g'_and_restore ln dest cont regenv exp
  | Let((x, t) as xt, exp, e, ln) ->
      assert (not (M.mem x regenv));
      let cont' = concat e dest cont in
      let (e1', regenv1) = g'_and_restore ln xt cont' regenv exp in
      let (_call, targets) = target x dest cont' in
      let sources = source t e1' in
      (* �쥸�����֤�mov�������𤹤�swap�Τۤ�������ʤΤǡ�sources���targets��ͥ�� *)
      (match alloc cont' regenv1 x t (targets @ sources) with
      | Spill(y) ->
          let r = M.find y regenv1 in
          let (e2', regenv2) = g ln dest cont (add x r (M.remove y regenv1)) e in
          let save =
            try Save(M.find y regenv, y, ln)
            with Not_found -> Nop in            
          (seq(save, concat e1' (r, t) e2', ln), regenv2)
      | Alloc(r) ->
          let (e2', regenv2) = g ln dest cont (add x r regenv1) e in
          (concat e1' (r, t) e2', regenv2))
and g'_and_restore ln dest cont regenv exp = (* ���Ѥ�����ѿ��򥹥��å�����쥸������Restore (caml2html: regalloc_unspill) *)
  try g' ln dest cont regenv exp
  with NoReg(x, t) ->
    ((* Format.eprintf "restoring %s@." x; *)
     g ln dest cont regenv (Let((x, t), Restore(x, ln), Ans(exp, ln), ln))) 
and g' ln dest cont regenv = function (* ��̿��Υ쥸����������� (caml2html: regalloc_gprime) *)
  | Nop | Set _ | SetL _ | Comment _ | Restore _ as exp -> (Ans(exp, ln), regenv)
  | Mov(x, ln) -> (Ans(Mov(find x Type.Int regenv, ln), ln), regenv)
  | Neg(x, ln) -> (Ans(Neg(find x Type.Int regenv, ln), ln), regenv)
  | Add(x, y', ln) -> (Ans(Add(find x Type.Int regenv, find' y' regenv, ln), ln), regenv)
  | Sub(x, y', ln) -> (Ans(Sub(find x Type.Int regenv, find' y' regenv, ln), ln), regenv)
  | Ld(x, y', i, ln) -> (Ans(Ld(find x Type.Int regenv, find' y' regenv, i, ln), ln), regenv)
  | St(x, y, z', i, ln) -> (Ans(St(find x Type.Int regenv, find y Type.Int regenv, find' z' regenv, i, ln), ln), regenv)
  | FMovD(x, ln) -> (Ans(FMovD(find x Type.Float regenv, ln), ln), regenv)
  | FNegD(x, ln) -> (Ans(FNegD(find x Type.Float regenv,ln), ln), regenv)
  | FAddD(x, y, ln) -> (Ans(FAddD(find x Type.Float regenv, find y Type.Float regenv, ln), ln), regenv)
  | FSubD(x, y, ln) -> (Ans(FSubD(find x Type.Float regenv, find y Type.Float regenv, ln), ln), regenv)
  | FMulD(x, y, ln) -> (Ans(FMulD(find x Type.Float regenv, find y Type.Float regenv, ln), ln), regenv)
  | FDivD(x, y, ln) -> (Ans(FDivD(find x Type.Float regenv, find y Type.Float regenv, ln), ln), regenv)
  | LdDF(x, y', i, ln) -> (Ans(LdDF(find x Type.Int regenv, find' y' regenv, i, ln), ln), regenv)
  | StDF(x, y, z', i, ln) -> (Ans(StDF(find x Type.Float regenv, find y Type.Int regenv, find' z' regenv, i, ln), ln), regenv)
  | IfEq(x, y', e1, e2, ln) as exp -> g'_if ln dest cont regenv exp (fun e1' e2' -> IfEq(find x Type.Int regenv, find' y' regenv, e1', e2', ln)) e1 e2 ln
  | IfLE(x, y', e1, e2, ln) as exp -> g'_if ln dest cont regenv exp (fun e1' e2' -> IfLE(find x Type.Int regenv, find' y' regenv, e1', e2', ln)) e1 e2 ln
  | IfGE(x, y', e1, e2, ln) as exp -> g'_if ln dest cont regenv exp (fun e1' e2' -> IfGE(find x Type.Int regenv, find' y' regenv, e1', e2', ln)) e1 e2 ln
  | IfFEq(x, y, e1, e2, ln) as exp -> g'_if ln dest cont regenv exp (fun e1' e2' -> IfFEq(find x Type.Float regenv, find y Type.Float regenv, e1', e2', ln)) e1 e2 ln
  | IfFLE(x, y, e1, e2, ln) as exp -> g'_if ln dest cont regenv exp (fun e1' e2' -> IfFLE(find x Type.Float regenv, find y Type.Float regenv, e1', e2', ln)) e1 e2 ln
  | CallCls(x, ys, zs, ln) as exp ->
      if List.length ys > Array.length regs - 1 || List.length zs > Array.length fregs then
        failwith (Format.sprintf "cannot allocate registers for arugments to %s" x)
      else
        g'_call ln dest cont regenv exp (fun ys zs -> CallCls(find x Type.Int regenv, ys, zs, ln)) ys zs ln
  | CallDir(Id.L(x), ys, zs, ln) as exp ->
      if List.length ys > Array.length regs || List.length zs > Array.length fregs then
        failwith (Format.sprintf "cannot allocate registers for arugments to %s" x)
      else
        g'_call ln dest cont regenv exp (fun ys zs -> CallDir(Id.L(x), ys, zs, ln)) ys zs ln
  | Save(x, y, _) -> assert false
and g'_if ln dest cont regenv exp constr e1 e2 ln = (* if�Υ쥸����������� (caml2html: regalloc_if) *)
  let (e1', regenv1) = g ln dest cont regenv e1 in
  let (e2', regenv2) = g ln dest cont regenv e2 in
  let regenv' = (* ξ���˶��̤Υ쥸�����ѿ��������� *)
    List.fold_left
      (fun regenv' x ->
        try
          if is_reg x then regenv' else
          let r1 = M.find x regenv1 in
          let r2 = M.find x regenv2 in
          if r1 <> r2 then regenv' else
          M.add x r1 regenv'
        with Not_found -> regenv')
      M.empty
      (fv cont) in
  (List.fold_left
     (fun e x ->
       if x = fst dest || not (M.mem x regenv) || M.mem x regenv' then e else
       seq(Save(M.find x regenv, x, ln), e, ln)) (* �����Ǥʤ��ѿ���ʬ��ľ���˥����� *)
     (Ans(constr e1' e2', ln))
     (fv cont),
   regenv')
and g'_call ln dest cont regenv exp constr ys zs ln = (* �ؿ��ƤӽФ��Υ쥸����������� (caml2html: regalloc_call) *)
  (List.fold_left
     (fun e x ->
       if x = fst dest || not (M.mem x regenv) then e else
       seq(Save(M.find x regenv, x, ln), e, ln))
     (Ans(constr
            (List.map (fun y -> find y Type.Int regenv) ys)
            (List.map (fun z -> find z Type.Float regenv) zs), ln))
     (fv cont),
   M.empty)

let h { name = Id.L(x); args = ys; fargs = zs; body = e; ret = t; ln = i } = (* �ؿ��Υ쥸����������� (caml2html: regalloc_h) *)
  if List.length ys > Array.length regs || List.length zs > Array.length fregs then
    Format.eprintf "too many arguments for function %s@." x;
  let regenv = M.add x reg_cl M.empty in
  let (i, arg_regs, regenv) =
    List.fold_left
      (fun (i, arg_regs, regenv) y ->
        let r = regs.(i) in
        (i + 1,
         arg_regs @ [r],
         (assert (not (is_reg y));
          M.add y r regenv)))
      (0, [], regenv)
      ys in
  let (d, farg_regs, regenv) =
    List.fold_left
      (fun (d, farg_regs, regenv) z ->
        let fr = fregs.(d) in
        (d + 1,
         farg_regs @ [fr],
         (assert (not (is_reg z));
          M.add z fr regenv)))
      (0, [], regenv)
      zs in
  let a =
    match t with
    | Type.Unit -> Id.gentmp Type.Unit
    | Type.Float -> fregs.(0)
    | _ -> regs.(0) in
  let (e', regenv') = g i (a, t) (Ans(Mov(a, i), i)) regenv e in
  { name = Id.L(x); args = arg_regs; fargs = farg_regs; body = e'; ret = t; ln = i }

let f (Prog(data, fundefs, e)) = (* �ץ���������ΤΥ쥸����������� (caml2html: regalloc_f) *)
  Format.eprintf "register allocation: may take some time (up to a few minutes, depending on the size of functions)@.";
  let fundefs' = List.map h fundefs in
  let e', regenv' = g 0 (Id.gentmp Type.Unit, Type.Unit) (Ans(Nop, 0)) M.empty e in (* ここ0でいいのかわからん *)
  Prog(data, fundefs', e')
