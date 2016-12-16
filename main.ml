open Syntax
open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  (**
  let rec read_eval_print_loop env' decls =
  (* let (id, newenv, v) = eval_decl env decl in *)
  match decls with
  (* letの複数宣言のときはdeclのリスト *)
  | Decls((id,exp)::ds) -> 
    let (id, newenv, v) = eval_decl env' decl in
    (**Printf.printf "val %s = " id
    pp_val v
    print_newline()**)
    read_eval_print_loop env' ds
  | ->
     read_eval_print env' in
   *)
  try
    (*let decls = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in*)
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
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
    read_eval_print newenv
    (*   read_eval_print_loop env decls; *)
    
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

let _ = read_eval_print initial_env
