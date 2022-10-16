(* translation into assembly with infinite number of virtual registers *)

open Asm

let data = ref [] (* ��ư��������������ơ��֥� (caml2html: virtual_data) *)

let classify xts ini addf addi =
  List.fold_left
    (fun acc (x, t) ->
      match t with
      | Type.Unit -> acc
      | Type.Float -> addf acc x
      | _ -> addi acc x t)
    ini
    xts

let separate xts =
  classify
    xts
    ([], [])
    (fun (int, float) x -> (int, float @ [x]))
    (fun (int, float) x _ -> (int @ [x], float))

let expand xts ini addf addi =
  classify
    xts
    ini
    (fun (offset, acc) x ->
      let offset = align offset in
      (offset + 8, addf x offset acc))
    (fun (offset, acc) x t ->
      (offset + 4, addi x t offset acc))

(* 下からきた行番号を上へ *)
let rec g env = function (* ���β��ۥޥ��󥳡������� (caml2html: virtual_g) *)
  | Closure.Unit(ln) -> Ans(Nop, ln)
  | Closure.Int(i, ln) -> Ans(Set(i, ln), ln)
  | Closure.Float(d, ln) ->
      let l =
        try
          (* ���Ǥ�����ơ��֥�ˤ��ä�������� Cf. https://github.com/esumii/min-caml/issues/13 *)
          let (l, _) = List.find (fun (_, d') -> d = d') !data in
          l
        with Not_found ->
          let l = Id.L(Id.genid "l") in
          data := (l, d) :: !data;
          l in
      let x = Id.genid "l" in
      Let((x, Type.Int), SetL(l, ln), Ans(LdDF(x, C(0), 1, ln), ln), ln)
  | Closure.Neg(x, ln) -> Ans(Neg(x, ln), ln)
  | Closure.Add(x, y, ln) -> Ans(Add(x, V(y), ln), ln)
  | Closure.Sub(x, y, ln) -> Ans(Sub(x, V(y), ln), ln)
  | Closure.FNeg(x, ln) -> Ans(FNegD(x, ln), ln)
  | Closure.FAdd(x, y, ln) -> Ans(FAddD(x, y, ln), ln)
  | Closure.FSub(x, y, ln) -> Ans(FSubD(x, y, ln), ln)
  | Closure.FMul(x, y, ln) -> Ans(FMulD(x, y, ln), ln)
  | Closure.FDiv(x, y, ln) -> Ans(FDivD(x, y, ln), ln)
  | Closure.IfEq(x, y, e1, e2, ln) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfEq(x, V(y), g env e1, g env e2, ln), ln)
      | Type.Float -> Ans(IfFEq(x, y, g env e1, g env e2, ln), ln)
      | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLE(x, y, e1, e2, ln) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfLE(x, V(y), g env e1, g env e2, ln), ln)
      | Type.Float -> Ans(IfFLE(x, y, g env e1, g env e2, ln), ln)
      | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.Let((x, t1), e1, e2, _) ->
      let e1' = g env e1 in
      let e2' = g (M.add x t1 env) e2 in
      concat e1' (x, t1) e2'
  | Closure.Var(x, ln) ->
      (match M.find x env with
      | Type.Unit -> Ans(Nop, ln)
      | Type.Float -> Ans(FMovD(x, ln), ln)
      | _ -> Ans(Mov(x, ln), ln))
  | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2, ln) -> (* ��������������� (caml2html: virtual_makecls) *)
      (* Closure�Υ��ɥ쥹�򥻥åȤ��Ƥ��顢��ͳ�ѿ����ͤ򥹥ȥ� *)
      let e2' = g (M.add x t env) e2 in
      let offset, store_fv =
        expand
          (List.map (fun y -> (y, M.find y env)) ys)
          (4, e2')
          (fun y offset store_fv -> seq(StDF(y, x, C(offset), 1, ln), store_fv, ln))
          (fun y _ offset store_fv -> seq(St(y, x, C(offset), 1, ln), store_fv, ln)) in
      Let((x, t), Mov(reg_hp, ln),
          Let(
            (reg_hp, Type.Int), 
            Add(reg_hp, C(align offset), ln),
              (let z = Id.genid "l" in
              Let((z, Type.Int), SetL(l, ln),
                  seq(St(z, x, C(0), 1, ln),
                      store_fv, ln), ln)), ln) ,ln)
  | Closure.AppCls(x, ys, ln) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallCls(x, int, float, ln), ln)
  | Closure.AppDir(Id.L(x), ys, ln) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallDir(Id.L(x), int, float, ln), ln)
  | Closure.Tuple(xs, ln) -> (* �Ȥ����� (caml2html: virtual_tuple) *)
      let y = Id.genid "t" in
      let (offset, store) =
        expand
          (List.map (fun x -> (x, M.find x env)) xs)
          (0, Ans(Mov(y,ln ), ln))
          (fun x offset store -> seq(StDF(x, y, C(offset), 1, ln), store, ln))
          (fun x _ offset store -> seq(St(x, y, C(offset), 1, ln), store, ln)) in
      Let((y, Type.Tuple(List.map (fun x -> M.find x env) xs)), Mov(reg_hp, ln),
          Let((reg_hp, Type.Int), Add(reg_hp, C(align offset), ln),
              store, ln), ln)
  | Closure.LetTuple(xts, y, e2, ln) ->
      let s = Closure.fv e2 in
      let (offset, load) =
        expand
          xts
          (0, g (M.add_list xts env) e2)
          (fun x offset load ->
            if not (S.mem x s) then load else (* [XX] a little ad hoc optimization *)
            fletd(x, LdDF(y, C(offset), 1, ln), load, ln))
          (fun x t offset load ->
            if not (S.mem x s) then load else (* [XX] a little ad hoc optimization *)
            Let((x, t), Ld(y, C(offset), 1, ln), load, ln)) in
      load
  | Closure.Get(x, y, ln) -> (* ������ɤ߽Ф� (caml2html: virtual_get) *)
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop, ln)
      | Type.Array(Type.Float) -> Ans(LdDF(x, V(y), 8,ln), ln)
      | Type.Array(_) -> Ans(Ld(x, V(y), 4, ln), ln)
      | _ -> assert false)
  | Closure.Put(x, y, z, ln) ->
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop, ln)
      | Type.Array(Type.Float) -> Ans(StDF(z, x, V(y), 8, ln), ln)
      | Type.Array(_) -> Ans(St(z, x, V(y), 4, ln), ln)
      | _ -> assert false)
  | Closure.ExtArray(Id.L(x), ln) -> Ans(SetL(Id.L("min_caml_" ^ x), ln), ln)

(* �ؿ��β��ۥޥ��󥳡������� (caml2html: virtual_h) *)
let h { Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e; Closure.ln = i } =
  let (int, float) = separate yts in
  let (offset, load) =
    expand
      zts
      (4, g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e)
      (fun z offset load -> fletd(z, LdDF(x, C(offset), 1, i), load, i))
      (fun z t offset load -> Let((z, t), Ld(x, C(offset), 1, i), load, i)) in
  match t with
  | Type.Fun(_, t2) ->
      { name = Id.L(x); args = int; fargs = float; body = load; ret = t2; ln = i }
  | _ -> assert false

(* �ץ���������Τβ��ۥޥ��󥳡������� (caml2html: virtual_f) *)
let f (Closure.Prog(fundefs, e)) =
  data := [];
  let fundefs = List.map h fundefs in
  let e = g M.empty e in
  Prog(!data, fundefs, e)
