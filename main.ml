let limit = ref 1000

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
  if e = e' then e else
  iter (n - 1) e'


(* 中間結果をダンプする関数 *)
let dump_parsed oc e =
   Dump.f oc e 0;
   e
let dump_normalized oc e =
   Dump.g oc e 0;
   e

let lexbuf outchan l p n = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  Emit.f outchan
    (RegAlloc.f
       (Simm.f
          (Virtual.f
             (Closure.f
                (iter !limit
                   (Alpha.f
                      (dump_normalized n  (* 中間結果をダンプ *)
                        (KNormal.f
                          (Typing.f
                              (dump_parsed p (* 中間結果をダンプ *)
                                (Parser.exp Lexer.token l)))))))))))

(* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)
let string s = lexbuf stdout (Lexing.from_string s) stdout stdout

let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  let pchan = open_out (f ^ ".parsed") in (* parse用のchannelを開く*)
  let nchan = open_out (f ^ ".normalized") in (* normalized用のchannelを開く*)
  try
    lexbuf outchan(Lexing.from_channel inchan) pchan nchan;
    close_in inchan;
    close_out outchan;
    close_out pchan;
    close_out nchan;
  with e -> (close_in inchan; close_out outchan; close_out pchan; close_out nchan; raise e) (* 追加したchannelsを閉じるコードを追加*)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
