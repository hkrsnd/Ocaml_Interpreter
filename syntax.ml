(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of (id list) * exp (* fun id -> exp *)
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp

type program =
    Exp of exp
  | Decl of id * exp
  | Decls of (id * exp) list
  | RecDecl of id * id * exp
