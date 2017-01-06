open Syntax
    
exception Error of string
    
let err s = raise (Error s)
    
(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

(* subst -> ty -> ty *)
(* substは型変数と型のペアのリスト [(id1,ty1); (id2,ty2); ... (idn,tyn)] *)
let rec subst_type subs ty =
  (* substの中からvarの型情報を探しだす関数 *)
  let rec lookup_subst_type subs' var =
    (match subs' with
     | [] -> (false, TyInt);
     | s :: ss -> (match s with
         | (id1, ty1) -> if (id1 = var) then (true, ty1) else lookup_subst_type ss var;
       ))
  in
  (* 型を探すターゲットのtyについて *)
  (match ty with
   | TyInt -> TyInt
   | TyBool -> TyBool
   | TyFun (ty1, ty2) -> TyFun ((subst_type subs ty1), (subst_type subs ty2));
   | TyVar var ->
     (* ペアの中から型変数の型情報を探す *)
     let vartype =  lookup_subst_type subs var in
     match vartype with
       (true, ty1) -> subst_type subs ty1;
     | (false, _) -> ty;
  )
;;


let ty_prim op ty1 ty2 = match op with
    Plus -> (match ty1, ty2 with
        TyInt, TyInt -> TyInt
      | _ -> err ("Argument must be of integer: +"))
  | Mult -> (match ty1, ty2 with
        TyInt, TyInt -> TyInt
      | _ -> err ("Argument must be of integer: *"))
  | Or -> (match ty1, ty2 with
        TyBool, TyBool -> TyBool
      | _ -> err ("Argument must be of bool: *"))
  | And -> (match ty1, ty2 with
        TyBool, TyBool -> TyBool
      | _ -> err ("Argument must be of bool: *"))
  | Lt -> (match ty1, ty2 with
        TyBool, TyBool -> TyBool
      | _ -> err ("Argument must be of bool: *"))
(*| Cons -> err "Not Implemented!"*)
              
let rec ty_exp tyenv = function
    Var x ->
    (try Environment.lookup x tyenv with
       Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
    let tyarg1 = ty_exp tyenv exp1 in
    let tyarg2 = ty_exp tyenv exp2 in
    ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) ->
    let tyarg1 = ty_exp tyenv exp1 in
    let tyarg2 = ty_exp tyenv exp2 in
    let tyarg3 = ty_exp tyenv exp3 in
    (match tyarg1, tyarg2, tyarg3 with
     | TyBool, type1, type2 ->
       if type1 == type2 then type1
       else err("Type mismatch at if then and else.")
     | _, _, _ -> err ("The condition must be boolean.")
    )
  | LetExp (id, exp1, exp2) ->
    let tyval1 = ty_exp tyenv exp1 in
    let newtyenv = Environment.extend id tyval1 tyenv in
    ty_exp newtyenv exp2
  | _ -> err ("Not Implemented!")

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err ("Not Implemented!")
