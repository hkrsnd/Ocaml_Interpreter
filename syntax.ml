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

(*ty -> tyvar MySet.t *)
let rec freevar_ty ty =
  let initial_set = MySet.empty in
  let rec freevar_ty_loop ty set =
    match ty with
    | TyFun(arg1, arg2) ->
      let newset = freevar_ty_loop arg1 set in
      freevar_ty_loop arg2 newset
    | TyVar tyv ->
      MySet.insert ty set
    | _ -> set
  in
  freevar_ty_loop ty MySet.empty

(*             
let rec pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar ty -> print_string (string_of_int ty)
  | TyFun (ty1 , ty2) -> pp_ty ty1; print_string " -> "; pp_ty ty2; print_newline ();
;;
 *)

(* tyvar: int　をkey, その型を表すシンボル(a'など))を値にもつ写像TyVarEnv *)                  
module TyVarEnv = Map.Make(struct type t = int let compare = compare end);;
let pp_ty ty =
  (* 各変数と対応するシンボルのMapを返す　(1 * "a'") など *)
    let make_tyvar_env ty fvars symbols =
      let rec make_tyvar_env_loop ty fvars symbols tyvarenv =
        match fvars with
          [] -> tyvarenv
        | v :: vs ->
           let newenv = TyVarEnv.add v (List.hd symbols) tyvarenv in
           make_tyvar_env_loop ty vs (List.tl symbols) newenv  in
      make_tyvar_env_loop ty fvars symbols TyVarEnv.empty
    in
  (* TyVar tyvarのセットからtyvarのリストを取り出す *)  
  let vars_to_int vars =
    List.map (fun x -> match x with TyVar tyvar -> tyvar) (MySet.to_list vars)  in
  
  let rec pp_ty_loop ty tyvarenv funcount =
    match ty with
      TyInt -> print_string "int"
    | TyBool -> print_string "bool"
    | TyVar tyvar -> print_string (TyVarEnv.find tyvar tyvarenv)
    | TyFun (ty1 , ty2) ->
       if (funcount <= 0) then
         begin 
           pp_ty_loop ty1 tyvarenv (funcount + 1);
           print_string " -> ";
           pp_ty_loop ty2 tyvarenv (funcount + 1);
         end
       else
         begin
         print_string "(";
         pp_ty_loop ty1 tyvarenv (funcount + 1); print_string " -> "; pp_ty_loop ty2 tyvarenv (funcount + 1);
         print_string ")";
         end
  in

  let env = make_tyvar_env ty (vars_to_int (freevar_ty ty)) ["a'";"b'";"c'";"d'";"e'";"f'";"g'"] in
  pp_ty_loop ty env 0
;;
  
  
let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body


  
type program =
    Exp of exp
  | Decl of id * exp
  | Decls of (id * exp) list
  | RecDecl of id * id * exp
