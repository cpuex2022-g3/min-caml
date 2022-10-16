(* type inference/reconstruction *)

open Syntax

exception Unify of Type.t * Type.t * Lexing.position (* 今読んでいるpositionもエレメントとして追加 *)
(* exception Error of t * Type.t * Type.t *)
exception Error of string * Type.t * Type.t 

let extenv = ref M.empty

(* for pretty printing (and type normalization) *)
let rec deref_typ = function (* 型変数を中身でおきかえる関数 (caml2html: typing_deref) *)
  | Type.Fun(t1s, t2) -> Type.Fun(List.map deref_typ t1s, deref_typ t2)
  | Type.Tuple(ts) -> Type.Tuple(List.map deref_typ ts)
  | Type.Array(t) -> Type.Array(deref_typ t)
  | Type.Var({ contents = None } as r) ->
      Format.eprintf "uninstantiated type variable detected; assuming int@.";
      r := Some(Type.Int);
      Type.Int
  | Type.Var({ contents = Some(t) } as r) ->
      let t' = deref_typ t in
      r := Some(t');
      t'
  | t -> t
let rec deref_id_typ (x, t) = (x, deref_typ t)
let rec deref_term = function
  | Not(e, pos) -> Not(deref_term e, pos)
  | Neg(e, pos) -> Neg(deref_term e, pos)
  | Add(e1, e2, pos) -> Add(deref_term e1, deref_term e2, pos)
  | Sub(e1, e2, pos) -> Sub(deref_term e1, deref_term e2, pos)
  | Eq(e1, e2,pos) -> Eq(deref_term e1, deref_term e2,pos)
  | LE(e1, e2,pos) -> LE(deref_term e1, deref_term e2,pos)
  | FNeg(e,pos) -> FNeg(deref_term e,pos)
  | FAdd(e1, e2,pos) -> FAdd(deref_term e1, deref_term e2,pos)
  | FSub(e1, e2,pos) -> FSub(deref_term e1, deref_term e2,pos)
  | FMul(e1, e2,pos) -> FMul(deref_term e1, deref_term e2,pos)
  | FDiv(e1, e2,pos) -> FDiv(deref_term e1, deref_term e2,pos)
  | If(e1, e2, e3, pos) -> If(deref_term e1, deref_term e2, deref_term e3, pos)
  | Let(xt, e1, e2,pos) -> Let(deref_id_typ xt, deref_term e1, deref_term e2,pos)
  | LetRec({ name = xt; args = yts; body = e1; ln = i }, e2,pos) ->
      LetRec({ name = deref_id_typ xt;
               args = List.map deref_id_typ yts;
               body = deref_term e1;
               ln   = i},
             deref_term e2,pos)
  | App(e, es,pos) -> App(deref_term e, List.map deref_term es,pos)
  | Tuple(es,pos) -> Tuple(List.map deref_term es,pos)
  | LetTuple(xts, e1, e2,pos) -> LetTuple(List.map deref_id_typ xts, deref_term e1, deref_term e2,pos)
  | Array(e1, e2,pos) -> Array(deref_term e1, deref_term e2,pos)
  | Get(e1, e2,pos) -> Get(deref_term e1, deref_term e2,pos)
  | Put(e1, e2, e3,pos) -> Put(deref_term e1, deref_term e2, deref_term e3,pos)
  | e -> e

let rec occur r1 = function (* occur check (caml2html: typing_occur) *)
  | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
  | Type.Tuple(t2s) -> List.exists (occur r1) t2s
  | Type.Array(t2) -> occur r1 t2
  | Type.Var(r2) when r1 == r2 -> true
  | Type.Var({ contents = None }) -> false
  | Type.Var({ contents = Some(t2) }) -> occur r1 t2
  | _ -> false

let rec unify pos t1 t2 = (* 型が合うように、型変数への代入をする (caml2html: typing_unify) *)
  match t1, t2 with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
      (try List.iter2 (unify pos) t1s t2s
      with Invalid_argument(_) -> raise (Unify(t1, t2, pos)));
      unify pos t1' t2'
  | Type.Tuple(t1s), Type.Tuple(t2s) ->
      (try List.iter2 (unify pos) t1s t2s
      with Invalid_argument(_) -> raise (Unify(t1, t2, pos)))
  | Type.Array(t1), Type.Array(t2) -> unify pos t1 t2
  | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> unify pos t1' t2
  | _, Type.Var({ contents = Some(t2') }) -> unify pos t1 t2'
  | Type.Var({ contents = None } as r1), _ -> (* 一方が未定義の型変数の場合 (caml2html: typing_undef) *)
      if occur r1 t2 then raise (Unify(t1, t2, pos));
      r1 := Some(t2)
  | _, Type.Var({ contents = None } as r2) ->
      if occur r2 t1 then raise (Unify(t1, t2, pos));
      r2 := Some(t1)
  | _, _ -> raise (Unify(t1, t2, pos))

(* Parserから来たSyntax.tに対応させるために、第2引数にposを追加 *)
(* 型エラーが起きるのはunify実行時なので、unifyにposを渡している *)
let rec g env e = (* 型推論ルーチン (caml2html: typing_g) *)
  try
    match e with
    | Unit(_) -> Type.Unit
    | Bool(_) -> Type.Bool
    | Int(_) -> Type.Int
    | Float(_) -> Type.Float
    | Not(e, pos) ->
        unify pos Type.Bool (g env e);
        Type.Bool
    | Neg(e, pos) ->
        unify pos Type.Int (g env e);
        Type.Int
    | Add(e1, e2, pos) | Sub(e1, e2, pos) -> (* 足し算（と引き算）の型推論 (caml2html: typing_add) *)
        unify pos Type.Int (g env e1);
        unify pos Type.Int (g env e2);
        Type.Int
    | FNeg(e, pos) ->
        unify pos Type.Float (g env e);
        Type.Float
    | FAdd(e1, e2, pos) | FSub(e1, e2, pos) | FMul(e1, e2, pos) | FDiv(e1, e2, pos) ->
        unify pos Type.Float (g env e1);
        unify pos Type.Float (g env e2);
        Type.Float
    | Eq(e1, e2, pos) | LE(e1, e2, pos) ->
        unify pos (g env e1) (g env e2);
        Type.Bool
    | If(e1, e2, e3, pos) ->
        unify pos (g env e1) Type.Bool;
        let t2 = g env e2 in
        let t3 = g env e3 in
        unify pos t2 t3;
        t2
    | Let((x, t), e1, e2, pos) -> (* letの型推論 (caml2html: typing_let) *)
        unify pos t (g env e1);
        g (M.add x t env) e2
    | Var(x, _) when M.mem x env -> M.find x env (* 変数の型推論 (caml2html: typing_var) *)
    | Var(x, _) when M.mem x !extenv -> M.find x !extenv
    | Var(x, _) -> (* 外部変数の型推論 (caml2html: typing_extvar) *)
        Format.eprintf "free variable %s assumed as external@." x;
        let t = Type.gentyp () in
        extenv := M.add x t !extenv;
        t
    | LetRec({ name = (x, t); args = yts; body = e1 }, e2, pos) -> (* let recの型推論 (caml2html: typing_letrec) *)
        let env = M.add x t env in
        unify pos t (Type.Fun(List.map snd yts, g (M.add_list yts env) e1));
        g env e2
    | App(e, es, pos) -> (* 関数適用の型推論 (caml2html: typing_app) *)
        let t = Type.gentyp () in
        unify pos (g env e) (Type.Fun(List.map (g env) es, t));
        t
    | Tuple(es, _) -> Type.Tuple(List.map (g env) es)
    | LetTuple(xts, e1, e2, pos) ->
        unify pos (Type.Tuple(List.map snd xts)) (g env e1);
        g (M.add_list xts env) e2
    | Array(e1, e2, pos) -> (* must be a primitive for "polymorphic" typing *)
        unify pos (g env e1) Type.Int;
        Type.Array(g env e2)
    | Get(e1, e2, pos) ->
        let t = Type.gentyp () in
        unify pos (Type.Array(t)) (g env e1);
        unify pos Type.Int (g env e2);
        t
    | Put(e1, e2, e3, pos) ->
        let t = g env e3 in
        unify pos (Type.Array(t)) (g env e1);
        unify pos Type.Int (g env e2);
        Type.Unit
  (* with Unify(t1, t2, pos) -> raise (Error(deref_term e, deref_typ t1, deref_typ t2)) *)
    with Unify(t1,t2, pos) -> raise (Error(Printf.sprintf "line %d" pos.pos_lnum, deref_typ t1, deref_typ t2))

let f e =
  extenv := M.empty;
(*
  (match deref_typ (g M.empty e) with
  | Type.Unit -> ()
  | _ -> Format.eprintf "warning: final result does not have type unit@.");
*)
  (try unify Lexing.dummy_pos Type.Unit (g M.empty e)
  with Unify _ -> failwith "top level does not have type unit");
  extenv := M.map deref_typ !extenv;
  deref_term e
