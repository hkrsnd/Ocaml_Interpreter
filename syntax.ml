(* ML interpreter / type reconstruction *)
type id = string
type tyvar = int
  
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

type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
             
let rec pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar ty -> print_string "var"
  | TyFun (ty1 , ty2) -> pp_ty ty1; print_string " -> "; pp_ty ty2; print_newline ();
;;
let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body

(*ty -> tyvar MySet.t *)
let rec freevar_ty ty =
  let initial_set = MySet.empty in
  let rec freevar_ty_loop ty set =
    match ty with
    | TyFun(arg1, arg2) ->
      let newset = freevar_ty_loop arg1 set in
      freevar_ty_loop arg2 newset
    | TyVar tyv ->
      MySet.insert tyv set
    | _ -> set
  in
  freevar_ty_loop ty MySet.empty

  
type program =
    Exp of exp
  | Decl of id * exp
  | Decls of (id * exp) list
  | RecDecl of id * id * exp
