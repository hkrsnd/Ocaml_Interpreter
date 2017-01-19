open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

(* subst -> ty -> ty *)
(* 型代入 -> 型変数を含む型 -> 型代入を使って型変数をできるだけ置き換えた型 *)
(* substは型変数と型のペアのリスト [(id1,ty1); (id2,ty2); ... (idn,tyn)] *)
(* let alpha = fresh_tyvar () in
   subst_type [(alpha, TyInt)] (TyFun (TyVar alpha, TyBool))
   の値は TyFun (TyInt, TyBool) *)
let rec subst_type subs ty =
  (* substの中からvarの型情報を探しだす関数 *)
  (* Option型のようなペアを返す *)
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

(* 型代入を型の等式集合に変換 *)
(* subst((tyvar * ty) list) -> (ty * ty) list *)
(* TyVarに包むだけ？ *)
let eqs_of_subst s = List.map (fun x -> (TyVar (fst x), snd x)) s
;;

(* (alpha, ty) で等式関係集合pairsを更新する *)
(* (tyvar * ty) -> (ty * ty) list -> (ty * ty) list *)
let subst_eqs subst eqs =
  (* 代入されるα *)
  let alpha =  fst subst in
  let newty = snd subst in
  let rec subst_eqs_loop subst eqs = 
    match eqs with
      [] -> []
    | p :: ps -> (match p with
                    ty1, ty2 -> (match ty1 with
                                 | TyVar tyvar1 -> if (tyvar1 = alpha) then
                                                     (newty, ty2) :: subst_eqs_loop subst ps
                                                   else
                                                     p :: subst_eqs_loop subst ps
                                 | _ -> (match ty2 with
                                         | TyVar tyvar2 -> if (tyvar2 = alpha) then
                                                             (ty1, newty) :: subst_eqs_loop subst ps
                                                           else
                                                             p :: subst_eqs_loop subst ps
                                         | _ -> (ty1, ty2) :: subst_eqs_loop subst ps
                                        )
                                )
                                
                 )
  in
  subst_eqs_loop subst eqs
;;

(*
    | p :: ps -> (match p with
          ty1, ty2 -> if (ty1 = alpha) then
            begin
              if (ty2 = alpha) then
                (alpha, alpha) :: subst_eqs_loop subst ps
              else
                (alpha, ty2) :: subst_eqs_loop subst ps
            end
          else if (ty2 = alpha) then
            (ty1, alpha) :: subst_eqs_loop subst ps
          else
            (ty1, ty2) :: subst_eqs_loop subst ps
      )
               *)


(* 単一化アルゴリズム 型代入を返す *)
(* (ty * ty) list -> subst((tyvar*ty)list) *)
let rec unify l = match l with
    [] -> []
  | p :: ps -> (match p with
        TyInt, TyInt -> unify ps
      | TyBool, TyBool -> unify ps
      | TyVar var, ty -> let ftv_ty = freevar_ty ty in
        if (not (MySet.member (TyVar var) ftv_ty)) then
          let updated = subst_eqs (var, ty) ps in
          List.rev ((var, ty) :: (List.rev (unify updated)))
        else
          err ("Type mismatch1.")
      | ty, TyVar var -> let ftv_ty = freevar_ty ty in
        if (not (MySet.member (TyVar var) ftv_ty)) then
          let updated = subst_eqs (var, ty) ps in
          List.rev ((var, ty) :: (List.rev (unify updated)))
        else
          err ("Type mismatch2.")
      | TyFun(ty11, ty12), TyFun(ty21, ty22) ->
        let newpairs = [(ty11, ty21);(ty12, ty22)] in
        unify (List.append newpairs  ps)
      | _, _ -> err("Type mismatch in unify.")
    )

let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
(*| Cons -> err "Not Implemented!"*)

let rec ty_exp tyenv = function
    Var x ->
    (try ([], Environment.lookup x tyenv) with
       Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let (eqs3, ty) = ty_prim op ty1 ty2 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
    let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let (s3, ty3) = ty_exp tyenv exp3 in
    (* 条件式はBool型であるという等式情報 *)
    let eqs_condition = [(ty1, TyBool)] in
    (* 返り値それぞれは等しい型であるという等式情報 *)
    let eqs_values = [(ty2, ty3)] in
    let eqs = eqs_condition @ eqs_values @ (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) in
    let s4 = unify eqs in (s4, subst_type s4 ty2)
  | LetExp (id, exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
    let s3 = unify eqs in (s3, subst_type s3 ty2)
  | FunExp (ids, exp) ->
    (match ids with
       id :: [] ->
       let domty = TyVar (fresh_tyvar ()) in
       let s, ranty =
         ty_exp (Environment.extend id domty tyenv) exp in
       (s, TyFun (subst_type s domty, ranty))
     | i :: is ->
       ty_exp (Environment.extend i (TyVar (fresh_tyvar ())) tyenv) (FunExp (is, exp))
     | _ -> err ("Invalid number of arguments.")
    )
  | AppExp (exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let newvar1 = TyVar (fresh_tyvar ()) in
    let newvar2 = TyVar (fresh_tyvar ()) in
    (* 関数の引数の型と実際の引数の型が等しい情報 *)
    let eqs_fun = match ty1 with
        TyFun(t1, t2) -> [(t1, ty2)]
      | _ (*alpha*)-> (* 関数が変数の場合 *)
         (*let t1 = TyVar (fresh_tyvar ()) in
         let t2 = TyVar (fresh_tyvar ()) in*)
         [(ty1, TyFun(newvar1,newvar2));(newvar1, ty2)]
    in
(*      | _ -> err ("not function") in *)
    (* 関数の返り値の型を取り出す *)
    let return_ty = match ty1 with
        TyFun(t1, t2) -> t2
      | _ (*alpha*) -> newvar2
    in
         (*      | _ -> err ("not function") in*)
    let eqs = eqs_fun @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
    let s3 = unify eqs in (s3, subst_type s3 return_ty)
  | _ -> err ("Not Implemented!")

let rec ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | Decl (id, e) ->
     let (s1, ty1) = ty_exp tyenv e in
     ty_exp (Environment.extend id ty1 tyenv) e
  (*     ty_exp (Environment.extend id ty1 tyenv) e *)
  | Decls (decls) ->
     (match decls with
      | (id, e) :: [] -> let (s1, ty1) = ty_exp tyenv e in
                         ty_exp (Environment.extend id ty1 tyenv) e;
      | (id, e) :: ds -> let (s1, ty1) = ty_exp tyenv e in
                         let newtyenv = (Environment.extend id ty1 tyenv) in
                         ty_decl newtyenv (Decls (ds))
      | _ -> err("Invalid decls.")
     )
  | _ -> err ("Not Implemented!")
