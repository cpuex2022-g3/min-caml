
open Syntax

let indent_width = 2

let rec list_ss oc ss depth = 
  match ss with 
  | [] -> ()
  | y::ys -> Printf.fprintf oc "%*s\n" depth y; list_ss oc ys depth


let rec unary oc name e depth ln = Printf.fprintf oc "%*d:%s\n" depth ln name; f oc e (depth + indent_width)

and binary oc name e1 e2 depth ln = Printf.fprintf oc "%*d:%s\n" depth ln name ; f oc e1 (depth + indent_width) ; f oc e2 (depth + indent_width)

and ternary oc name e1 e2 e3 depth ln = Printf.fprintf oc "%*d:%s\n" depth ln name; f oc e1 (depth + indent_width); f oc e2 (depth + indent_width); f oc e3 (depth + indent_width) 

and list_es oc es depth = 
    match es with 
    | [] -> ()
    | y::ys -> f oc y depth; list_es oc ys depth


and f oc e depth = 
  match e with 
  | Unit(pos) -> Printf.fprintf oc "%*d:UNIT\n" depth pos.pos_lnum
  | Bool(b, pos) -> 
    if b then Printf.fprintf oc "%*d:BOOL TRUE\n" depth pos.pos_lnum
    else Printf.fprintf oc "%*d:BOOL FALSE\n" depth pos.pos_lnum
  | Int(i, pos) -> Printf.fprintf oc "%*d:INT %d\n" depth pos.pos_lnum i 
  | Float(d, pos) -> Printf.fprintf oc "%*d:FLOAT %f\n" depth pos.pos_lnum d
  | Not(e, pos) -> unary oc "NOT" e depth pos.pos_lnum
  | Neg(e, pos) -> unary oc "NEG" e depth pos.pos_lnum
  | Add(e1, e2, pos) -> binary oc "ADD" e1 e2 depth pos.pos_lnum 
  | Sub(e1, e2, pos) -> binary oc "SUB" e1 e2 depth pos.pos_lnum
  | FNeg(e, pos) ->
    unary oc "FNEG" e depth pos.pos_lnum
  | FAdd(e1, e2, pos) ->
    binary oc "ADD" e1 e2 depth pos.pos_lnum
  | FSub(e1, e2, pos) ->
    binary oc "FSUB" e1 e2 depth pos.pos_lnum
  | FMul(e1, e2, pos) -> binary oc "FMUL" e1 e2 depth pos.pos_lnum
  | FDiv(e1, e2, pos) -> binary oc "FDIV" e1 e2 depth pos.pos_lnum
  | Eq(e1, e2, pos) -> binary oc "EQ" e1 e2 depth pos.pos_lnum
  | LE(e1, e2, pos) -> binary oc "LE" e1 e2 depth pos.pos_lnum
  | If(e1, e2, e3, pos) ->
    ternary oc "IF" e1 e2 e3 depth pos.pos_lnum
  | Let((x, t), e1, e2, pos) -> 
    Printf.fprintf oc "%*d:LET %s\n" depth pos.pos_lnum x;
    f oc e1 (depth + indent_width);
    f oc e2 (depth + indent_width);
  | Var(x, pos) -> 
      Printf.fprintf oc "%*d:VAR %s\n" depth pos.pos_lnum x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2, pos) -> 
      Printf.fprintf oc "%*d:LETREC %s\n" depth pos.pos_lnum x;
      f oc e1 (depth + indent_width);
      f oc e2 (depth + indent_width);
  | App(e, es, pos) -> 
      Printf.fprintf oc "%*d:APP\n" depth pos.pos_lnum;
      f oc e (depth + indent_width);
      list_es oc es (depth+2)
  | Tuple(es, pos) -> 
      Printf.fprintf oc "%*d:TUPLE\n" depth pos.pos_lnum;
      list_es oc es (depth + indent_width)
  | LetTuple(xts, e1, e2, pos) ->
    Printf.fprintf oc "%*d:LETTUPLE\n" depth pos.pos_lnum;
    let _ = List.map (fun (x,_) -> Printf.fprintf oc "%*s\n" (depth + indent_width) x) xts in
    f oc e1 (depth + indent_width);
    f oc e2 (depth + indent_width);
  | Array(e1, e2, pos) -> 
      binary oc "ARRAY" e1 e2 depth pos.pos_lnum
  | Get(e1, e2, pos) ->
      binary oc "GET" e1 e2 depth pos.pos_lnum
  | Put(e1, e2, e3, pos) ->
      ternary oc "PUT" e1 e2 e3 depth pos.pos_lnum

open KNormal


let rec g oc e depth =
  match e with 
    | Unit(ln) -> Printf.fprintf oc "%*d:UNIT\n" depth ln
    | Int(i, ln) -> Printf.fprintf oc "%*d:INT %d\n" depth i ln
    | Float(d, ln) -> Printf.fprintf oc "%*d: FLOAT %f\n" depth ln d
    | Neg(e, ln) -> Printf.fprintf oc "%*d:NEG %s\n" depth ln e
    | Add(x, y, ln) -> Printf.fprintf oc "%*d:ADD %s %s\n" depth ln x y
    | Sub(x, y, ln) -> Printf.fprintf oc "%*d:SUB %s %s\n" depth ln x y
    | FNeg(x, ln) -> Printf.fprintf oc "%*d:FNEG %s\n" depth ln x
    | FAdd(x, y, ln) -> Printf.fprintf oc "%*d:FADD %s %s\n" depth ln x y
    | FSub(x, y, ln) ->  Printf.fprintf oc "%*d:FSUB %s %s\n" depth ln x y
    | FMul(x, y, ln) ->  Printf.fprintf oc "%*d:FMUL %s %s\n" depth ln x y
    | FDiv(x, y, ln) ->  Printf.fprintf oc "%*d:FDIV %s %s\n" depth ln x y
    | IfEq(x, y, e1, e2, ln) -> 
        Printf.fprintf oc "%*d:IFEQ %s %s\n" depth ln x y;
        g oc e1 (depth + indent_width);
        g oc e2 (depth + indent_width);
    | IfLE(x, y, e1, e2, ln) ->
        Printf.fprintf oc "%*d:IFLE %s %s\n" depth ln x y;
        g oc e1 (depth + indent_width);
        g oc e2 (depth + indent_width);
    | Let((x, t), e1, e2, ln) ->
        Printf.fprintf oc "%*d:LET %s\n" depth ln x;
        g oc e1 (depth + indent_width);
        g oc e2 (depth + indent_width);
    | Var(x, ln) -> Printf.fprintf oc "%*d:VAR %s\n" depth ln x
    | LetRec({ name = (x, t); args = yts; body = e1 }, e2, ln) ->
        Printf.fprintf oc "%*d:LETREC %s\n" depth ln x;
        g oc e1 (depth + indent_width);
        g oc e2 (depth+2)
    | App(x, ys, ln) ->
        Printf.fprintf oc "%*d:APP %s\n" depth ln x;
        list_ss oc ys (depth + indent_width); 
        Printf.fprintf oc "\n"
    | Tuple(xs, ln) -> 
        Printf.fprintf oc "%*d:TUPLE\n" depth ln;
        list_ss oc xs (depth + indent_width); 
        Printf.fprintf oc "\n"
    | LetTuple(xts, y, e, ln) -> 
        Printf.fprintf oc "%*d:LETTUPLE\n" depth ln;
        let _ = List.map (fun (x,_) -> Printf.fprintf oc "%s " x) xts in
        Printf.fprintf oc "\n"
    | Get(x, y, ln) -> 
        Printf.fprintf oc "%*d:GET\n" depth ln;
        Printf.fprintf oc "%*s\n" (depth + indent_width) x;
        Printf.fprintf oc "%*s\n" (depth + indent_width) y
    | Put(x, y, z, ln) -> 
        Printf.fprintf oc "%*d:PUT\n" depth ln;
        Printf.fprintf oc "%*s\n" (depth + indent_width) x;
        Printf.fprintf oc "%*s\n" (depth + indent_width) y;
        Printf.fprintf oc "%*s\n" (depth + indent_width) z
    | ExtArray(x, ln) -> Printf.fprintf oc "%*d:EXTARRAY %s\n" depth ln x
    | ExtFunApp(x, ys, ln) -> 
      Printf.fprintf oc "%*d:EXTFUNAPP %s\n" depth ln x;
      list_ss oc ys (depth + indent_width) 
