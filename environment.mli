type 'a t

exception Not_bound

val empty : 'a t
val extend : Syntax.id -> 'a -> 'a t -> 'a t
(* val extend_list: list Syntax.id -> list 'a -> 'a t -> 'a t*)
val extend_pairs : 'a list -> 'a list -> 'a list
val lookup : Syntax.id -> 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
