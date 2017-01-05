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
  | REC
  | INTV of (int)
  | ID of (Syntax.id)
  | IDS of (Syntax.id list)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Syntax
# 30 "parser.ml"
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
  275 (* REC *);
    0|]

let yytransl_block = [|
  276 (* INTV *);
  277 (* ID *);
  278 (* IDS *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\002\000\
\002\000\002\000\002\000\002\000\002\000\009\000\004\000\004\000\
\008\000\010\000\006\000\006\000\011\000\011\000\007\000\007\000\
\012\000\012\000\013\000\013\000\014\000\014\000\015\000\015\000\
\015\000\015\000\015\000\005\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\005\000\009\000\004\000\005\000\001\000\
\001\000\001\000\001\000\001\000\001\000\004\000\002\000\001\000\
\006\000\010\000\003\000\001\000\003\000\001\000\003\000\001\000\
\003\000\001\000\003\000\001\000\002\000\001\000\001\000\001\000\
\001\000\001\000\003\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\032\000\033\000\000\000\000\000\
\031\000\034\000\037\000\000\000\008\000\000\000\000\000\011\000\
\012\000\013\000\000\000\000\000\000\000\000\000\030\000\000\000\
\000\000\000\000\000\000\000\000\016\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\029\000\000\000\000\000\035\000\
\000\000\000\000\000\000\000\000\000\000\015\000\022\000\000\000\
\021\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\000\000\004\000\000\000\036\000\000\000\000\000\
\017\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\007\000\018\000"

let yydgoto = "\002\000\
\011\000\012\000\067\000\030\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000"

let yysindex = "\010\000\
\004\255\000\000\019\255\019\255\000\000\000\000\247\254\024\255\
\000\000\000\000\000\000\033\255\000\000\042\255\000\000\000\000\
\000\000\000\000\034\255\015\255\047\255\046\255\000\000\030\255\
\052\255\045\255\035\255\243\254\000\000\021\255\000\000\046\255\
\046\255\046\255\046\255\046\255\000\000\039\255\048\255\000\000\
\019\255\054\255\019\255\244\254\019\255\000\000\000\000\034\255\
\000\000\047\255\058\255\046\255\055\255\019\255\057\255\056\255\
\020\255\019\255\000\000\059\255\060\255\019\255\051\255\000\000\
\061\255\019\255\073\255\000\000\063\255\000\000\068\255\062\255\
\000\000\000\000\070\255\019\255\019\255\019\255\255\254\074\255\
\075\255\000\000\019\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\103\000\053\000\000\000\
\000\000\000\000\083\000\063\000\029\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\093\000\
\000\000\043\000\073\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\086\255\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\003\000\011\000\064\000\000\000\000\000\250\255\000\000\
\000\000\000\000\062\000\060\000\065\000\061\000\234\255"

let yytablesize = 374
let yytable = "\037\000\
\028\000\082\000\043\000\058\000\003\000\025\000\026\000\029\000\
\046\000\027\000\001\000\028\000\004\000\083\000\027\000\005\000\
\006\000\007\000\034\000\003\000\035\000\008\000\064\000\009\000\
\010\000\047\000\049\000\004\000\026\000\037\000\005\000\006\000\
\024\000\065\000\066\000\031\000\008\000\045\000\009\000\010\000\
\033\000\046\000\025\000\055\000\029\000\057\000\003\000\059\000\
\038\000\032\000\039\000\036\000\010\000\040\000\041\000\042\000\
\061\000\005\000\006\000\053\000\068\000\034\000\024\000\054\000\
\070\000\009\000\010\000\062\000\073\000\056\000\060\000\071\000\
\023\000\063\000\066\000\074\000\069\000\077\000\079\000\080\000\
\081\000\072\000\020\000\075\000\076\000\085\000\078\000\065\000\
\006\000\083\000\084\000\044\000\019\000\048\000\051\000\000\000\
\052\000\000\000\050\000\000\000\000\000\000\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\000\000\028\000\028\000\000\000\000\000\028\000\028\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\000\000\
\027\000\027\000\000\000\000\000\027\000\027\000\026\000\026\000\
\026\000\000\000\026\000\026\000\026\000\000\000\026\000\026\000\
\000\000\000\000\026\000\026\000\025\000\025\000\025\000\000\000\
\025\000\025\000\025\000\000\000\025\000\025\000\010\000\010\000\
\025\000\025\000\000\000\022\000\022\000\000\000\010\000\010\000\
\024\000\024\000\010\000\010\000\000\000\024\000\024\000\000\000\
\024\000\024\000\023\000\023\000\024\000\024\000\000\000\023\000\
\023\000\000\000\023\000\023\000\020\000\020\000\023\000\023\000\
\000\000\000\000\020\000\000\000\020\000\020\000\019\000\019\000\
\020\000\020\000\000\000\000\000\019\000\000\000\019\000\019\000\
\009\000\009\000\019\000\019\000\000\000\000\000\000\000\000\000\
\009\000\009\000\000\000\000\000\009\000\009\000"

let yycheck = "\022\000\
\000\000\003\001\016\001\016\001\001\001\003\000\004\000\021\001\
\021\001\019\001\001\000\021\001\009\001\015\001\000\000\012\001\
\013\001\014\001\004\001\001\001\006\001\018\001\003\001\020\001\
\021\001\032\000\033\000\009\001\000\000\052\000\012\001\013\001\
\014\001\014\001\015\001\003\001\018\001\017\001\020\001\021\001\
\007\001\021\001\000\000\041\000\021\001\043\000\001\001\045\000\
\019\001\008\001\021\001\005\001\000\000\002\001\010\001\021\001\
\054\000\012\001\013\001\021\001\058\000\004\001\000\000\016\001\
\062\000\020\001\021\001\011\001\066\000\016\001\016\001\021\001\
\000\000\018\001\015\001\003\001\018\001\016\001\076\000\077\000\
\078\000\021\001\000\000\021\001\017\001\083\000\017\001\014\001\
\003\001\015\001\080\000\028\000\000\000\032\000\035\000\255\255\
\036\000\255\255\034\000\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\255\255\010\001\011\001\255\255\255\255\014\001\015\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\255\255\
\010\001\011\001\255\255\255\255\014\001\015\001\002\001\003\001\
\004\001\255\255\006\001\007\001\008\001\255\255\010\001\011\001\
\255\255\255\255\014\001\015\001\002\001\003\001\004\001\255\255\
\006\001\007\001\008\001\255\255\010\001\011\001\002\001\003\001\
\014\001\015\001\255\255\007\001\008\001\255\255\010\001\011\001\
\002\001\003\001\014\001\015\001\255\255\007\001\008\001\255\255\
\010\001\011\001\002\001\003\001\014\001\015\001\255\255\007\001\
\008\001\255\255\010\001\011\001\002\001\003\001\014\001\015\001\
\255\255\255\255\008\001\255\255\010\001\011\001\002\001\003\001\
\014\001\015\001\255\255\255\255\008\001\255\255\010\001\011\001\
\002\001\003\001\014\001\015\001\255\255\255\255\255\255\255\255\
\010\001\011\001\255\255\255\255\014\001\015\001"

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
  REC\000\
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
# 21 "parser.mly"
                    ( Exp _1 )
# 254 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 22 "parser.mly"
                                ( Decl (_2, _4) )
# 262 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'LetOps) in
    Obj.repr(
# 23 "parser.mly"
                                       ( Decls([(_2,_4)] @ _5) )
# 271 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'IDs) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 24 "parser.mly"
                           ( Decl (_2,FunExp(_3, _5)) )
# 280 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : Syntax.id) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 25 "parser.mly"
                                                  ( RecDecl(_3,_6,_8) )
# 289 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 28 "parser.mly"
                     ( [(_2, _4)] )
# 297 "parser.ml"
               : 'LetOps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'LetOps) in
    Obj.repr(
# 29 "parser.mly"
                              ( [(_2, _4)] @ _5 )
# 306 "parser.ml"
               : 'LetOps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'IfExpr) in
    Obj.repr(
# 32 "parser.mly"
               ( _1 )
# 313 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'OrExpr) in
    Obj.repr(
# 33 "parser.mly"
                 ( _1 )
# 320 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LTExpr) in
    Obj.repr(
# 34 "parser.mly"
                 ( _1 )
# 327 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LetExpr) in
    Obj.repr(
# 35 "parser.mly"
                  ( _1 )
# 334 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'FunExpr) in
    Obj.repr(
# 36 "parser.mly"
                  ( _1 )
# 341 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LetRecExpr) in
    Obj.repr(
# 37 "parser.mly"
                     ( _1 )
# 348 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'IDs) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 40 "parser.mly"
                                ( FunExp(_2, _4) )
# 356 "parser.ml"
               : 'FunExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'IDs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 42 "parser.mly"
         ( _1 @ [_2])
# 364 "parser.ml"
               : 'IDs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 43 "parser.mly"
             ( [_1] )
# 371 "parser.ml"
               : 'IDs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 45 "parser.mly"
                              ( LetExp (_2, _4, _6) )
# 380 "parser.ml"
               : 'LetExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : Syntax.id) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 47 "parser.mly"
                                                  ( LetRecExp(_3, _6, _8,_10))
# 390 "parser.ml"
               : 'LetRecExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'OrExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AndExpr) in
    Obj.repr(
# 50 "parser.mly"
                       ( BinOp (Or, _1, _3) )
# 398 "parser.ml"
               : 'OrExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AndExpr) in
    Obj.repr(
# 51 "parser.mly"
               ( _1 )
# 405 "parser.ml"
               : 'OrExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AndExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'LTExpr) in
    Obj.repr(
# 54 "parser.mly"
                           ( BinOp (And, _1, _3) )
# 413 "parser.ml"
               : 'AndExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LTExpr) in
    Obj.repr(
# 55 "parser.mly"
                 ( _1 )
# 420 "parser.ml"
               : 'AndExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 58 "parser.mly"
                        ( BinOp (Lt, _1, _3) )
# 428 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 59 "parser.mly"
                 ( _1 )
# 435 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 62 "parser.mly"
                                 ( BinOp (Plus, _1, _3) )
# 443 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 63 "parser.mly"
                        ( _1 )
# 450 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 66 "parser.mly"
                         ( BinOp (Mult, _1, _3) )
# 458 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 67 "parser.mly"
                ( _1 )
# 465 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 70 "parser.mly"
                        ( AppExp (_1, _2) )
# 473 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 71 "parser.mly"
                  ( _1 )
# 480 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
         ( ILit _1 )
# 487 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
         ( BLit true )
# 493 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
          ( BLit false )
# 499 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 77 "parser.mly"
       ( Var _1 )
# 506 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 78 "parser.mly"
                       ( _2 )
# 513 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 81 "parser.mly"
                                ( IfExp (_2, _4, _6) )
# 522 "parser.ml"
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
