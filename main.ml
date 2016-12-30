open Syntax
open Eval
open Printf

(* declと環境を受け取って、評価して結果を出力する。返り値は評価後の新しい環境 *)
let eval_and_print_decl env decl =
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
            pp_val v;
            print_newline();            
            print_ids_and_values is vs;
         end
    in
    print_ids_and_values ids vs;
    newenv
  ;;
(* 適当な出力をしながらeval_and_print_declを呼び出す。 *)
let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
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
    let newenv = eval_and_print_decl env decl in
    read_eval_print newenv;
    
  with
  | Failure string ->  Printf.printf "%s \n" string; read_eval_print env
  | Eval.Error string -> Printf.printf "%s \n" string; read_eval_print env

     
let initial_env = 
  Environment.extend "i" (IntV 1)
  (Environment.extend "ii" (IntV 2)
    (Environment.extend "iii" (IntV 3)
      (Environment.extend "iv" (IntV 4)
        (Environment.extend "v" (IntV 5) 
          (Environment.extend "x" (IntV 10) Environment.empty)))))

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
  
(* stringのリストを受け取り、プログラムなら評価しコメントなら無視する *)
let rec eval_batch_strings env strs =
  match strs with
  | [] -> print_endline;
  | s :: ss -> let decl = Parser.toplevel Lexer.main (Lexing.from_string s) in
               let newenv = eval_and_print_decl env decl in
               eval_batch_strings newenv ss;
;;
(* stringのリストからコメント部分を切り取って返す *)
let cut_comments strs =
  let comment_start_regex = regex "(\*.*" in
  let comment_end_regex = regex ".*\*)" in
  
  let rec cut_comments_loop strs depth =
    match strs with
    | [] -> []
    | s :: ss ->
       if depth > 0 then
         if string_match comment_end_regex s then
           
           
  match strs with
    
(* ファイル名と環境を受けとり、ファイルの内容を評価する *)
let get_and_eval_from_batch_file env filename =
  let strs = get_strings_from_batch_file filename in
  eval_batch_strings env strs
;;
let () =
  match Sys.argv with
    (* コマンドライン引数なしのとき *)
  | [|ocaml|] -> read_eval_print initial_env
  (* コマンドライン引数としてファイル名１つを受け取る場合 *)
  | [|ocaml; file|] ->
     get_and_eval_from_batch_file initial_env file; ();
  | _ -> read_eval_print initial_env
;;
