open Syntax
open Eval
open Printf
open Typing

(* declと環境を受け取って、評価して結果を出力する。返り値は評価後の新しい環境 *)
let eval_and_print_decl env decl ty =
  let (ids, newenv, vs) = eval_decl env decl in
  let rec print_ids_and_values ids values =
    match ids with
    | []-> ();
    | i :: is ->
      begin
        match values with
        | [] -> ();
        | v :: vs ->
          Printf.printf "val %s = " i;
          pp_ty ty;
          print_string " = ";
          pp_val v;
          print_newline();
          print_ids_and_values is vs;
      end
  in
  print_ids_and_values ids vs;
  newenv
;;
(* 適当な出力をしながらeval_and_print_declを呼び出す。 *)
let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let ty = ty_decl tyenv decl in
    (**let (ids, newenv, vs) = eval_decl env decl in
       let rec print_ids_and_values ids values =
       match ids with
       | []-> ();
       | i :: is ->
         begin
         match values with
         | [] -> ();
         | v :: vs ->
            Printf.printf "val %s = " i;
            pp_val v;
            print_newline();            
            print_ids_and_values is vs;
         end
       in
       print_ids_and_values ids vs;**)
    let newenv = eval_and_print_decl env decl ty in
    read_eval_print newenv tyenv;
  with
  | Failure string ->  Printf.printf "%s \n" string; read_eval_print env tyenv
  | Eval.Error string -> Printf.printf "%s \n" string; read_eval_print env tyenv
      
let initial_env =
  Environment.empty
let initial_tyenv = 
  Environment.extend "i" TyInt
    (Environment.extend "v" TyInt
       (Environment.extend "x" TyInt Environment.empty))

(* let _ = read_eval_print initial_env*)


(**
   let ic = open_in filename in
   try
   while true do
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel ic) in
    let newenv = eval_and_print_decl env decl in
    ();
   done
   with End_of_file ->
   close_in ic;
 **)
;;

(* 外部ファイルから一行ずつ読み込み、stringのリストを返す *)
let get_strings_from_batch_file filename =
  let ic = open_in filename in
  let rec get_strings_loop ic strs =
    try
      let line = input_line ic in
      get_strings_loop ic (strs @ [line]);
    with End_of_file ->
      close_in ic;
      strs;
  in
  get_strings_loop ic []
;;

(* search_forward reg s num で見つからなかった時例外を投げずに-1を返すようにラップした関数 *)
let search_string reg s =
  try Str.search_forward reg s 0
  with Not_found ->
    -1
;;
(* stringのリストをsemisemiごとにまとめる *)
let gather_by_semisemi strs =
  let semisemi_regexp = Str.regexp ".*;;" in
  (* results: list string *)
  let rec gather_by_semisemi_loop strs str =
    match strs with
    | [] -> []
    | s :: ss ->
      (* ;;がある場合 *)
      if (search_string semisemi_regexp s >= 0) then
        (* ;;までを１つのストリングとしてまとめ、再帰的に後の要素を求める *)
        let before_semisemi = Str.string_before s (Str.match_end ()) in
        (str ^ " " ^ before_semisemi) :: (gather_by_semisemi_loop ss "")
      else
        (* ;;がない場合は今までのstrの後にそのままappend *)
        gather_by_semisemi_loop ss (str ^ s)
  in
  gather_by_semisemi_loop strs ""
;;

(* stringのリストを受け取り、プログラムなら評価しコメントなら無視する *)
let rec eval_batch_strings env tyenv strs =
  match strs with
  | [] -> print_endline;
  | s :: ss -> let decl = Parser.toplevel Lexer.main (Lexing.from_string s) in
    let ty = ty_decl tyenv decl in
    let newenv = eval_and_print_decl env decl ty in
    eval_batch_strings newenv tyenv ss;
;;
(* stringのリストからコメント部分を切り取って返す *)
let cut_comments strs =
  let comment_start_regex = Str.regexp "(\\*.*" in
  let comment_end_regex = Str.regexp ".*\\*)" in
  (*let comment_one_linear_regex = Str.regexp "\(\*.*\*\)" in *)

  (* 再帰呼出しする再帰関数 *)
  let rec cut_comments_loop strs depth  =
    match strs with
    | [] -> []
    | s :: ss ->
      (* コメント内部のとき *)
      if depth > 0 then (* コメントの始まり記号* ) があるとき *)
        if (search_string comment_start_regex s >= 0) then
          (* depthを増やして始まり記号以降を再探索 *)
          let begin_pos = Str.match_beginning () in
          cut_comments_loop ((Str.string_after s (begin_pos+2)) :: ss) (depth+1)
          (* コメントの終わり記号* ) があったとき *)
        else if (search_string comment_end_regex s >= 0) then
          (* depth=1のとき、* ) の後はコメントでなくプログラム *)
          if (depth = 1) then
            (* 終わり記号の後が改行かどうか *)
            if (Str.match_end () = String.length s) then begin
              cut_comments_loop ss (depth - 1)
            end
            else
              let not_comment_part = (Str.string_after s (Str.match_end ())) in
              not_comment_part :: (cut_comments_loop ss (depth - 1))
          else
            (* depth >= 2のとき、まだコメント *)
            let after_endcomment_part = (Str.string_after s (Str.match_end ())) in
            cut_comments_loop (after_endcomment_part :: ss) (depth - 1)
        else
          (* コメントの終わり記号がないとき *)
          (* そのまま次の行へ *)
          cut_comments_loop ss depth
      else begin
        (* コメント外部のとき *)
        (* コメント始まり記号があったらそれまではプログラム、それ以降はコメント *)
        if (search_string comment_start_regex s >= 0) then
          let begin_pos = Str.match_beginning () in
          (Str.string_before s begin_pos) :: (cut_comments_loop ((Str.string_after s (begin_pos+2)) :: ss) (depth+1))
        else
          (* コメント始まり記号がない場合、その行すべてがプログラム *)
          s :: (cut_comments_loop ss depth)
      end
  in
  let rec remove_null strs =
    match strs with
    | [] -> []
    | s :: ss ->
      if (s = "") then
        remove_null ss
      else
        s :: remove_null ss
  in
  let strs_with_null = cut_comments_loop strs 0 in
  remove_null strs_with_null
;;
(* ファイル名と環境を受けとり、ファイルの内容を評価する *)
let get_and_eval_from_batch_file env tyenv filename =
  let strs = get_strings_from_batch_file filename in
  let strs_without_comments = cut_comments strs in
  let gathered_strs = gather_by_semisemi strs_without_comments in
  eval_batch_strings env tyenv gathered_strs
;;
let () =
  match Sys.argv with
  (* コマンドライン引数なしのとき *)
  | [|ocaml|] -> read_eval_print initial_env initial_tyenv
  (* コマンドライン引数としてファイル名１つを受け取る場合 *)
  | [|ocaml; file|] ->
    get_and_eval_from_batch_file initial_env initial_tyenv file; ();
  | _ -> read_eval_print initial_env initial_tyenv
;;
