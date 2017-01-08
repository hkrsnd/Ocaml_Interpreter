open Typing
open Syntax
(*
let test = 
  let alpha = fresh_tyvar () in
  pp_ty (subst_type [(alpha, TyInt)] (TyFun (TyVar alpha, TyBool)));
  let alpha = fresh_tyvar () in
  let beta = fresh_tyvar () in
  pp_ty (subst_type [(beta, (TyFun (TyVar alpha, TyInt))); (alpha, TyBool)] (TyVar beta));
;;
 *)
let test =
  let ls = [(TyInt, TyInt); (TyVar 1, TyBool)] in
  unify ls
;;
