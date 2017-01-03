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
\002\000\002\000\002\000\008\000\004\000\004\000\009\000\009\000\
\010\000\010\000\007\000\006\000\006\000\013\000\013\000\014\000\
\014\000\012\000\012\000\009\000\009\000\010\000\010\000\011\000\
\011\000\011\000\011\000\011\000\005\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\008\000\005\000\004\000\005\000\001\000\
\001\000\001\000\001\000\004\000\002\000\001\000\003\000\001\000\
\002\000\001\000\006\000\003\000\001\000\003\000\001\000\003\000\
\001\000\003\000\001\000\003\000\001\000\002\000\001\000\001\000\
\001\000\001\000\001\000\003\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\033\000\034\000\000\000\000\000\
\032\000\035\000\038\000\000\000\008\000\009\000\010\000\011\000\
\000\000\000\000\018\000\000\000\000\000\000\000\000\000\000\000\
\000\000\014\000\000\000\001\000\000\000\017\000\000\000\000\000\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\005\000\037\000\000\000\000\000\019\000\003\000\000\000\000\000\
\004\000\000\000\007\000"

let yydgoto = "\002\000\
\011\000\012\000\056\000\027\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\000\000\000\000"

let yysindex = "\009\000\
\007\255\000\000\055\255\055\255\000\000\000\000\005\255\238\254\
\000\000\000\000\000\000\008\255\000\000\000\000\000\000\000\000\
\012\255\050\255\000\000\026\255\010\255\031\255\024\255\014\255\
\244\254\000\000\001\255\000\000\050\255\000\000\050\255\050\255\
\020\255\000\000\055\255\022\255\055\255\247\254\055\255\000\000\
\050\255\012\255\036\255\055\255\033\255\027\255\255\254\055\255\
\000\000\032\255\055\255\021\255\000\000\034\255\055\255\045\255\
\000\000\000\000\035\255\038\255\000\000\000\000\055\255\055\255\
\000\000\044\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\029\000\001\000\000\000\049\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\000\043\000\059\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\057\255\000\000"

let yygindex = "\000\000\
\000\000\002\000\251\255\047\000\000\000\000\000\000\000\000\000\
\046\000\045\000\238\255\048\000\000\000\000\000"

let yytablesize = 330
let yytable = "\030\000\
\016\000\053\000\026\000\037\000\022\000\023\000\048\000\003\000\
\026\000\001\000\028\000\040\000\054\000\055\000\015\000\004\000\
\029\000\039\000\005\000\006\000\007\000\040\000\030\000\024\000\
\008\000\025\000\009\000\010\000\027\000\031\000\033\000\032\000\
\034\000\035\000\036\000\044\000\045\000\046\000\047\000\031\000\
\049\000\059\000\026\000\051\000\052\000\050\000\055\000\062\000\
\021\000\057\000\003\000\063\000\058\000\064\000\060\000\003\000\
\061\000\054\000\020\000\006\000\067\000\005\000\006\000\004\000\
\065\000\066\000\005\000\006\000\021\000\009\000\010\000\038\000\
\008\000\041\000\009\000\010\000\042\000\000\000\000\000\043\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\016\000\016\000\016\000\016\000\000\000\
\000\000\000\000\016\000\016\000\000\000\000\000\016\000\016\000\
\015\000\015\000\015\000\015\000\015\000\000\000\000\000\000\000\
\015\000\015\000\000\000\000\000\015\000\015\000\027\000\027\000\
\027\000\000\000\027\000\000\000\000\000\000\000\027\000\027\000\
\000\000\000\000\027\000\027\000\026\000\026\000\026\000\000\000\
\026\000\000\000\021\000\021\000\026\000\026\000\000\000\000\000\
\026\000\026\000\021\000\021\000\020\000\020\000\021\000\021\000\
\000\000\000\000\000\000\000\000\020\000\020\000\000\000\000\000\
\020\000\020\000"

let yycheck = "\018\000\
\000\000\003\001\021\001\016\001\003\000\004\000\016\001\001\001\
\021\001\001\000\003\001\021\001\014\001\015\001\000\000\009\001\
\005\001\017\001\012\001\013\001\014\001\021\001\041\000\019\001\
\018\001\021\001\020\001\021\001\000\000\004\001\021\001\006\001\
\002\001\010\001\021\001\016\001\035\000\016\001\037\000\004\001\
\039\000\021\001\000\000\011\001\018\001\044\000\015\001\003\001\
\000\000\048\000\001\001\017\001\051\000\016\001\021\001\001\001\
\055\000\014\001\000\000\003\001\066\000\012\001\013\001\009\001\
\063\000\064\000\012\001\013\001\014\001\020\001\021\001\025\000\
\018\001\029\000\020\001\021\001\031\000\255\255\255\255\032\000\
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
# 238 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 22 "parser.mly"
                            ( Decl (_2, _4) )
# 246 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'LetOps) in
    Obj.repr(
# 23 "parser.mly"
                                   ( Decls([(_2,_4)] @ _5) )
# 255 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : Syntax.id) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Syntax.id) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 24 "parser.mly"
                                     ( RecDecl(_3,_6,_8) )
# 264 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'IDs) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 25 "parser.mly"
                       ( Decl (_2,FunExp(_3, _5)) )
# 273 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 28 "parser.mly"
                     ( [(_2, _4)] )
# 281 "parser.ml"
               : 'LetOps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'LetOps) in
    Obj.repr(
# 29 "parser.mly"
                              ( [(_2, _4)] @ _5 )
# 290 "parser.ml"
               : 'LetOps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'IfExpr) in
    Obj.repr(
# 32 "parser.mly"
               ( _1 )
# 297 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LTExpr) in
    Obj.repr(
# 33 "parser.mly"
                 ( _1 )
# 304 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LetExpr) in
    Obj.repr(
# 34 "parser.mly"
                  ( _1 )
# 311 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'FunExpr) in
    Obj.repr(
# 35 "parser.mly"
                  ( _1 )
# 318 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'IDs) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 38 "parser.mly"
                                ( FunExp(_2, _4) )
# 326 "parser.ml"
               : 'FunExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'IDs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 40 "parser.mly"
         ( _1 @ [_2])
# 334 "parser.ml"
               : 'IDs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 41 "parser.mly"
             ( [_1] )
# 341 "parser.ml"
               : 'IDs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 43 "parser.mly"
                               ( BinOp (Mult, _1, _3) )
# 349 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 44 "parser.mly"
                      ( _1 )
# 356 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 46 "parser.mly"
                              ( AppExp (_1, _2) )
# 364 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 47 "parser.mly"
                        ( _1 )
# 371 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 49 "parser.mly"
                              ( LetExp (_2, _4, _6) )
# 380 "parser.ml"
               : 'LetExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 52 "parser.mly"
                        ( BinOp (Lt, _1, _3) )
# 388 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 53 "parser.mly"
                 ( _1 )
# 395 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 56 "parser.mly"
                        ( BinOp (And, _1, _3) )
# 403 "parser.ml"
               : 'ANDExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 57 "parser.mly"
                ( _1 )
# 410 "parser.ml"
               : 'ANDExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 60 "parser.mly"
                    ( BinOp (Or, _1, _3) )
# 418 "parser.ml"
               : 'ORExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 61 "parser.mly"
                    ( _1 )
# 425 "parser.ml"
               : 'ORExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 64 "parser.mly"
                     ( BinOp (Plus, _1, _3) )
# 433 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 65 "parser.mly"
          ( _1 )
# 440 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 68 "parser.mly"
                         ( BinOp (Mult, _1, _3) )
# 448 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 69 "parser.mly"
                ( _1 )
# 455 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 72 "parser.mly"
                        ( AppExp (_1, _2) )
# 463 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 73 "parser.mly"
                  ( _1 )
# 470 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 76 "parser.mly"
         ( ILit _1 )
# 477 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
         ( BLit true )
# 483 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
          ( BLit false )
# 489 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 79 "parser.mly"
       ( Var _1 )
# 496 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 80 "parser.mly"
                       ( _2 )
# 503 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 83 "parser.mly"
                                ( IfExp (_2, _4, _6) )
# 512 "parser.ml"
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
