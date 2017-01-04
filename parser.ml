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
\002\000\002\000\002\000\002\000\008\000\004\000\004\000\010\000\
\010\000\011\000\011\000\007\000\009\000\006\000\006\000\014\000\
\014\000\015\000\015\000\013\000\013\000\010\000\010\000\011\000\
\011\000\012\000\012\000\012\000\012\000\012\000\005\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\008\000\005\000\004\000\005\000\001\000\
\001\000\001\000\001\000\001\000\004\000\002\000\001\000\003\000\
\001\000\002\000\001\000\006\000\010\000\003\000\001\000\003\000\
\001\000\003\000\001\000\003\000\001\000\003\000\001\000\002\000\
\001\000\001\000\001\000\001\000\001\000\003\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\035\000\036\000\000\000\000\000\
\034\000\037\000\040\000\000\000\008\000\009\000\010\000\011\000\
\012\000\000\000\000\000\019\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\000\000\001\000\000\000\018\000\000\000\
\000\000\000\000\000\000\038\000\000\000\000\000\000\000\000\000\
\000\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\005\000\000\000\039\000\000\000\
\000\000\020\000\003\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\000\021\000"

let yydgoto = "\002\000\
\011\000\012\000\060\000\028\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\021\000\000\000\000\000"

let yysindex = "\026\000\
\005\255\000\000\021\255\021\255\000\000\000\000\248\254\251\254\
\000\000\000\000\000\000\025\255\000\000\000\000\000\000\000\000\
\000\000\026\255\043\255\000\000\041\255\032\255\030\255\027\255\
\031\255\245\254\000\000\243\254\000\000\043\255\000\000\043\255\
\043\255\036\255\044\255\000\000\021\255\045\255\021\255\247\254\
\021\255\000\000\043\255\026\255\058\255\049\255\021\255\037\255\
\048\255\006\255\021\255\000\000\053\255\052\255\021\255\051\255\
\000\000\054\255\021\255\070\255\000\000\055\255\000\000\060\255\
\062\255\000\000\000\000\063\255\021\255\021\255\021\255\064\255\
\067\255\064\255\021\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\001\000\000\000\049\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\043\000\059\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\082\000\
\080\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\011\000\060\000\000\000\000\000\000\000\000\000\
\000\000\053\000\057\000\237\255\055\000\000\000\000\000"

let yytablesize = 330
let yytable = "\031\000\
\017\000\023\000\024\000\041\000\039\000\003\000\051\000\042\000\
\057\000\027\000\025\000\042\000\026\000\004\000\016\000\027\000\
\005\000\006\000\007\000\058\000\059\000\003\000\008\000\031\000\
\009\000\010\000\001\000\029\000\029\000\004\000\030\000\036\000\
\005\000\006\000\022\000\048\000\037\000\050\000\008\000\052\000\
\009\000\010\000\028\000\003\000\032\000\054\000\033\000\055\000\
\023\000\061\000\034\000\038\000\035\000\063\000\005\000\006\000\
\046\000\066\000\022\000\047\000\049\000\032\000\009\000\010\000\
\053\000\056\000\059\000\072\000\073\000\074\000\062\000\064\000\
\067\000\077\000\065\000\068\000\069\000\070\000\075\000\071\000\
\058\000\004\000\006\000\076\000\044\000\040\000\043\000\045\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\017\000\017\000\017\000\017\000\000\000\
\000\000\000\000\017\000\017\000\000\000\000\000\017\000\017\000\
\016\000\016\000\016\000\016\000\016\000\000\000\000\000\000\000\
\016\000\016\000\000\000\000\000\016\000\016\000\029\000\029\000\
\029\000\000\000\029\000\000\000\000\000\000\000\029\000\029\000\
\000\000\000\000\029\000\029\000\028\000\028\000\028\000\000\000\
\028\000\000\000\023\000\023\000\028\000\028\000\000\000\000\000\
\028\000\028\000\023\000\023\000\022\000\022\000\023\000\023\000\
\000\000\000\000\000\000\000\000\022\000\022\000\000\000\000\000\
\022\000\022\000"

let yycheck = "\019\000\
\000\000\003\000\004\000\017\001\016\001\001\001\016\001\021\001\
\003\001\021\001\019\001\021\001\021\001\009\001\000\000\021\001\
\012\001\013\001\014\001\014\001\015\001\001\001\018\001\043\000\
\020\001\021\001\001\000\003\001\000\000\009\001\005\001\002\001\
\012\001\013\001\014\001\037\000\010\001\039\000\018\001\041\000\
\020\001\021\001\000\000\001\001\004\001\047\000\006\001\011\001\
\000\000\051\000\019\001\021\001\021\001\055\000\012\001\013\001\
\021\001\059\000\000\000\016\001\016\001\004\001\020\001\021\001\
\016\001\018\001\015\001\069\000\070\000\071\000\018\001\021\001\
\003\001\075\000\021\001\021\001\017\001\016\001\015\001\017\001\
\014\001\000\000\003\001\073\000\032\000\026\000\030\000\033\000\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\255\255\010\001\011\001\255\255\255\255\014\001\015\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\255\255\
\010\001\011\001\255\255\255\255\014\001\015\001\002\001\003\001\
\004\001\255\255\006\001\255\255\255\255\255\255\010\001\011\001\
\255\255\255\255\014\001\015\001\002\001\003\001\004\001\255\255\
\006\001\255\255\002\001\003\001\010\001\011\001\255\255\255\255\
\014\001\015\001\010\001\011\001\002\001\003\001\014\001\015\001\
\255\255\255\255\255\255\255\255\010\001\011\001\255\255\255\255\
\014\001\015\001"

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
# 241 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 22 "parser.mly"
                            ( Decl (_2, _4) )
# 249 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'LetOps) in
    Obj.repr(
# 23 "parser.mly"
                                   ( Decls([(_2,_4)] @ _5) )
# 258 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : Syntax.id) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Syntax.id) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 24 "parser.mly"
                                     ( RecDecl(_3,_6,_8) )
# 267 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'IDs) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 25 "parser.mly"
                       ( Decl (_2,FunExp(_3, _5)) )
# 276 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 28 "parser.mly"
                     ( [(_2, _4)] )
# 284 "parser.ml"
               : 'LetOps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'LetOps) in
    Obj.repr(
# 29 "parser.mly"
                              ( [(_2, _4)] @ _5 )
# 293 "parser.ml"
               : 'LetOps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'IfExpr) in
    Obj.repr(
# 32 "parser.mly"
               ( _1 )
# 300 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LTExpr) in
    Obj.repr(
# 33 "parser.mly"
                 ( _1 )
# 307 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LetExpr) in
    Obj.repr(
# 34 "parser.mly"
                  ( _1 )
# 314 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'FunExpr) in
    Obj.repr(
# 35 "parser.mly"
                  ( _1 )
# 321 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LetRecExpr) in
    Obj.repr(
# 36 "parser.mly"
                     ( _1 )
# 328 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'IDs) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 39 "parser.mly"
                                ( FunExp(_2, _4) )
# 336 "parser.ml"
               : 'FunExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'IDs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 41 "parser.mly"
         ( _1 @ [_2])
# 344 "parser.ml"
               : 'IDs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 42 "parser.mly"
             ( [_1] )
# 351 "parser.ml"
               : 'IDs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 44 "parser.mly"
                               ( BinOp (Mult, _1, _3) )
# 359 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 45 "parser.mly"
                      ( _1 )
# 366 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 47 "parser.mly"
                              ( AppExp (_1, _2) )
# 374 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 48 "parser.mly"
                        ( _1 )
# 381 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 50 "parser.mly"
                              ( LetExp (_2, _4, _6) )
# 390 "parser.ml"
               : 'LetExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : Syntax.id) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 52 "parser.mly"
                                               ( LetRecExp(_3, _6, _8,_10))
# 400 "parser.ml"
               : 'LetRecExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 55 "parser.mly"
                        ( BinOp (Lt, _1, _3) )
# 408 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 56 "parser.mly"
                 ( _1 )
# 415 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 59 "parser.mly"
                        ( BinOp (And, _1, _3) )
# 423 "parser.ml"
               : 'ANDExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 60 "parser.mly"
                ( _1 )
# 430 "parser.ml"
               : 'ANDExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 63 "parser.mly"
                    ( BinOp (Or, _1, _3) )
# 438 "parser.ml"
               : 'ORExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 64 "parser.mly"
                    ( _1 )
# 445 "parser.ml"
               : 'ORExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 67 "parser.mly"
                     ( BinOp (Plus, _1, _3) )
# 453 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 68 "parser.mly"
          ( _1 )
# 460 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 71 "parser.mly"
                         ( BinOp (Mult, _1, _3) )
# 468 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 72 "parser.mly"
                ( _1 )
# 475 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 75 "parser.mly"
                        ( AppExp (_1, _2) )
# 483 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 76 "parser.mly"
                  ( _1 )
# 490 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 79 "parser.mly"
         ( ILit _1 )
# 497 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
         ( BLit true )
# 503 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
          ( BLit false )
# 509 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 82 "parser.mly"
       ( Var _1 )
# 516 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 83 "parser.mly"
                       ( _2 )
# 523 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 86 "parser.mly"
                                ( IfExp (_2, _4, _6) )
# 532 "parser.ml"
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
