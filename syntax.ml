(* ML interpreter / type reconstruction *)

type tyvar = int
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

type id = string

(* type scheme *)
type tysc = TyScheme of tyvar list * ty
let tysc_of_ty ty = TyScheme ([], ty)
                             
exception Error of string
let err s = raise (Error s)
                  
(* 型スキームからその中の自由な型変数 ( の集合 ) を求める関数 *)
let freevar_tysc tysc =
  (* 内部関数 *)
  let rec freevar_tysc_loop vars ty set =
    (match ty with
     | TyFun(arg1, arg2) ->
        let newset = freevar_tysc_loop vars arg1 set in
        freevar_tysc_loop vars arg2 newset
     | TyVar tyv ->
        (* 変数αが束縛されている場合を除く *)
        if(not (List.memq tyv vars)) then
          MySet.insert ty set
        else
          set
     | _ -> set) in
  (* 初期状態の環境 *)
  let initial_set = MySet.empty in
  
  (* 再帰関数呼び出し *)
  match tysc with
  |(vars, ty1) ->
    freevar_tysc_loop vars ty1 MySet.empty
  | _ -> err ("Internal error: Invalid typescheme.")
    

                      
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
