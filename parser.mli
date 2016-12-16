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

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.program
