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
(*let test =
  let ls = [(TyInt, TyInt); (TyVar 1, TyBool)] in
  unify ls
;;*)
let test =
  let ty = TyFun(TyVar 1, TyInt) in
  let ftv_ty = freevar_ty ty in
  (*  print_string (string_of_int (MySet.len ftv_ty)); *)

  if (MySet.member 1 ftv_ty) then
       print_string "yes"
  else
    print_string "no"


(*
match  (List.hd (MySet.to_list ftv_ty)) with
                                   | TyVar num ->  print_string (string_of_int num);
 *)
;;
                                   
