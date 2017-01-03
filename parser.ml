type token =
  | LPAREN
  | RPAREN
  | SEMISEMI
  | PLUS
  | MULT
  | LT
  | AND
  | OR
  | IF
  | THEN
  | ELSE
  | TRUE
  | FALSE
  | LET
  | IN
  | EQ
  | RARROW
  | FUN
  | INTV of (int)
  | ID of (Syntax.id)
  | IDS of (Syntax.id list)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Syntax
# 29 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* SEMISEMI *);
  260 (* PLUS *);
  261 (* MULT *);
  262 (* LT *);
  263 (* AND *);
  264 (* OR *);
  265 (* IF *);
  266 (* THEN *);
  267 (* ELSE *);
  268 (* TRUE *);
  269 (* FALSE *);
  270 (* LET *);
  271 (* IN *);
  272 (* EQ *);
  273 (* RARROW *);
  274 (* FUN *);
    0|]

let yytransl_block = [|
  275 (* INTV *);
  276 (* ID *);
  277 (* IDS *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\002\000\002\000\002\000\
\002\000\007\000\008\000\008\000\009\000\009\000\006\000\005\000\
\005\000\012\000\012\000\013\000\013\000\011\000\011\000\008\000\
\008\000\009\000\009\000\010\000\010\000\010\000\010\000\010\000\
\004\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\004\000\005\000\001\000\001\000\001\000\
\001\000\004\000\003\000\001\000\002\000\001\000\006\000\003\000\
\001\000\003\000\001\000\003\000\001\000\003\000\001\000\003\000\
\001\000\002\000\001\000\001\000\001\000\001\000\001\000\003\000\
\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\029\000\030\000\000\000\000\000\
\028\000\031\000\034\000\000\000\006\000\007\000\008\000\009\000\
\000\000\000\000\014\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\013\000\000\000\000\000\000\000\032\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\010\000\000\000\000\000\002\000\000\000\000\000\000\000\
\033\000\000\000\015\000\003\000\000\000\000\000\005\000"

let yydgoto = "\002\000\
\011\000\012\000\048\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\000\000\000\000"

let yysindex = "\006\000\
\001\255\000\000\015\255\015\255\000\000\000\000\244\254\245\254\
\000\000\000\000\000\000\008\255\000\000\000\000\000\000\000\000\
\007\255\036\255\000\000\255\254\005\255\021\255\016\255\022\255\
\023\255\000\000\036\255\000\000\036\255\036\255\026\255\000\000\
\015\255\015\255\015\255\036\255\007\255\035\255\015\255\032\255\
\003\255\000\000\030\255\015\255\000\000\027\255\015\255\043\255\
\000\000\037\255\000\000\000\000\015\255\038\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\083\255\055\255\000\000\103\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\069\255\097\255\113\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\048\255\000\000"

let yygindex = "\000\000\
\000\000\253\255\008\000\000\000\000\000\000\000\000\000\025\000\
\036\000\242\255\034\000\000\000\000\000"

let yytablesize = 128
let yytable = "\022\000\
\023\000\003\000\029\000\028\000\030\000\045\000\001\000\024\000\
\025\000\004\000\026\000\027\000\005\000\006\000\007\000\003\000\
\046\000\047\000\008\000\009\000\010\000\028\000\032\000\004\000\
\031\000\033\000\005\000\006\000\021\000\040\000\041\000\042\000\
\008\000\009\000\010\000\043\000\003\000\034\000\029\000\035\000\
\049\000\039\000\044\000\051\000\047\000\052\000\050\000\005\000\
\006\000\054\000\004\000\046\000\053\000\037\000\009\000\010\000\
\012\000\012\000\012\000\012\000\012\000\055\000\036\000\038\000\
\012\000\012\000\000\000\000\000\012\000\012\000\011\000\011\000\
\011\000\011\000\011\000\000\000\000\000\000\000\011\000\011\000\
\000\000\000\000\011\000\011\000\023\000\023\000\023\000\000\000\
\023\000\000\000\000\000\000\000\023\000\023\000\000\000\000\000\
\023\000\023\000\022\000\022\000\022\000\000\000\022\000\000\000\
\017\000\017\000\022\000\022\000\000\000\000\000\022\000\022\000\
\017\000\017\000\016\000\016\000\017\000\017\000\000\000\000\000\
\000\000\000\000\016\000\016\000\000\000\000\000\016\000\016\000"

let yycheck = "\003\000\
\004\000\001\001\004\001\018\000\006\001\003\001\001\000\020\001\
\020\001\009\001\003\001\005\001\012\001\013\001\014\001\001\001\
\014\001\015\001\018\001\019\001\020\001\036\000\002\001\009\001\
\020\001\010\001\012\001\013\001\014\001\033\000\034\000\035\000\
\018\001\019\001\020\001\039\000\001\001\016\001\004\001\017\001\
\044\000\016\001\011\001\047\000\015\001\003\001\020\001\012\001\
\013\001\053\000\003\001\014\001\016\001\029\000\019\001\020\001\
\002\001\003\001\004\001\005\001\006\001\054\000\027\000\030\000\
\010\001\011\001\255\255\255\255\014\001\015\001\002\001\003\001\
\004\001\005\001\006\001\255\255\255\255\255\255\010\001\011\001\
\255\255\255\255\014\001\015\001\002\001\003\001\004\001\255\255\
\006\001\255\255\255\255\255\255\010\001\011\001\255\255\255\255\
\014\001\015\001\002\001\003\001\004\001\255\255\006\001\255\255\
\002\001\003\001\010\001\011\001\255\255\255\255\014\001\015\001\
\010\001\011\001\002\001\003\001\014\001\015\001\255\255\255\255\
\255\255\255\255\010\001\011\001\255\255\255\255\014\001\015\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  SEMISEMI\000\
  PLUS\000\
  MULT\000\
  LT\000\
  AND\000\
  OR\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  LET\000\
  IN\000\
  EQ\000\
  RARROW\000\
  FUN\000\
  "

let yynames_block = "\
  INTV\000\
  ID\000\
  IDS\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 20 "parser.mly"
                ( Exp _1 )
# 177 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 21 "parser.mly"
                            ( Decl (_2, _4) )
# 185 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'LetOps) in
    Obj.repr(
# 22 "parser.mly"
                                   ( Decls([(_2,_4)] @ _5) )
# 194 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 25 "parser.mly"
                     ( [(_2, _4)] )
# 202 "parser.ml"
               : 'LetOps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'LetOps) in
    Obj.repr(
# 26 "parser.mly"
                              ( [(_2, _4)] @ _5 )
# 211 "parser.ml"
               : 'LetOps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'IfExpr) in
    Obj.repr(
# 29 "parser.mly"
               ( _1 )
# 218 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LTExpr) in
    Obj.repr(
# 30 "parser.mly"
                 ( _1 )
# 225 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LetExpr) in
    Obj.repr(
# 31 "parser.mly"
                  ( _1 )
# 232 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'FunExpr) in
    Obj.repr(
# 32 "parser.mly"
                  ( _1 )
# 239 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 35 "parser.mly"
                               ( FunExp(_2, _4) )
# 247 "parser.ml"
               : 'FunExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 37 "parser.mly"
                               ( BinOp (Mult, _1, _3) )
# 255 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 38 "parser.mly"
                      ( _1 )
# 262 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 40 "parser.mly"
                              ( AppExp (_1, _2) )
# 270 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 41 "parser.mly"
                        ( _1 )
# 277 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 43 "parser.mly"
                              ( LetExp (_2, _4, _6) )
# 286 "parser.ml"
               : 'LetExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 46 "parser.mly"
                        ( BinOp (Lt, _1, _3) )
# 294 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 47 "parser.mly"
                 ( _1 )
# 301 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 50 "parser.mly"
                        ( BinOp (And, _1, _3) )
# 309 "parser.ml"
               : 'ANDExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 51 "parser.mly"
                ( _1 )
# 316 "parser.ml"
               : 'ANDExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 54 "parser.mly"
                    ( BinOp (Or, _1, _3) )
# 324 "parser.ml"
               : 'ORExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 55 "parser.mly"
                    ( _1 )
# 331 "parser.ml"
               : 'ORExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 58 "parser.mly"
                     ( BinOp (Plus, _1, _3) )
# 339 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 59 "parser.mly"
          ( _1 )
# 346 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 62 "parser.mly"
                         ( BinOp (Mult, _1, _3) )
# 354 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 63 "parser.mly"
                ( _1 )
# 361 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 66 "parser.mly"
                        ( AppExp (_1, _2) )
# 369 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 67 "parser.mly"
                  ( _1 )
# 376 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 70 "parser.mly"
         ( ILit _1 )
# 383 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
         ( BLit true )
# 389 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
          ( BLit false )
# 395 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 73 "parser.mly"
       ( Var _1 )
# 402 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 74 "parser.mly"
                       ( _2 )
# 409 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 77 "parser.mly"
                                ( IfExp (_2, _4, _6) )
# 418 "parser.ml"
               : 'IfExpr))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.program)
