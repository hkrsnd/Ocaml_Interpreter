%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token RARROW FUN
%token REC

%token <int> INTV
%token <Syntax.id> ID
%token <Syntax.id list> IDS

%start toplevel
%type <Syntax.program> toplevel
%%
  
toplevel :
  Expr SEMISEMI { Exp $1 }
  | LET ID EQ Expr SEMISEMI { Decl ($2, $4) }
  | LET ID EQ Expr LetOps SEMISEMI { Decls([($2,$4)] @ $5) }
  | LET REC ID EQ FUN ID RARROW Expr { RecDecl($3,$6,$8) }
  | LET ID IDs EQ Expr { Decl ($2,FunExp($3, $5)) }

LetOps :
      LET ID EQ Expr { [($2, $4)] }
      | LET ID EQ Expr LetOps { [($2, $4)] @ $5 }
    
Expr :
        IfExpr { $1 }
        | LTExpr { $1 }
        | LetExpr { $1 }
        | FunExpr { $1 }

FunExpr :
            FUN IDs RARROW Expr { FunExp($2, $4) }
IDs   :
  IDs ID { $1 @ [$2]}
        | ID { [$1] }
MExpr :
            MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
            | AppExpr { $1 }
AppExpr :
                AppExpr AExpr { AppExp ($1, $2) }
                | AExpr { $1 }
LetExpr :
       LET ID EQ Expr IN Expr { LetExp ($2, $4, $6) }
    
LTExpr :
         PExpr LT PExpr { BinOp (Lt, $1, $3) }
         | PExpr { $1 }
    
ANDExpr :
        PExpr AND PExpr { BinOp (And, $1, $3) }
        | PExpr { $1 }
  
ORExpr :
     PExpr OR PExpr { BinOp (Or, $1, $3) }
            | PExpr { $1 }
  
PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }

MExpr :
      MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
      | AppExpr { $1 }
    
AppExpr :
          AppExpr AExpr { AppExp ($1, $2) }
          | AExpr { $1 }
    
AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }
