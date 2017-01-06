type 'a t = (Syntax.id * 'a) list

exception Not_bound

exception Error of string
let err s = raise (Error s)
let empty = []
let extend x v env = (x,v)::env

let rec extend_list xl vl env = match xl with
  | [] -> env
  | x::xs -> match vl with
             | [] -> err ("Internal Error: The number of env-id and env-value is different to extend the env.")
             | v::vs -> (extend_list xs vs ((x,v)::env))

let rec extend_pairs pairs env = match pairs with
  | [] -> env
  | p::ps -> (extend_pairs ps (p :: env))
                      
let rec lookup x env = 
  try List.assoc x env with Not_found -> raise Not_bound

let rec map f = function
    [] -> []
  | (id, v)::rest -> (id, f v) :: map f rest

let rec fold_right f env a = 
  match env with
      [] -> a
    | (_, v)::rest -> f v (fold_right f rest a)
