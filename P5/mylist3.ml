(*remove : 'a -> 'a list -> 'a list, elimina primera aparicion (si la hay), de un valor de una lista*)
let rec remove v=function
    []->[]
    |h::t -> if v=h then t
             else h::remove v t;; (*val remove : 'a -> 'a list -> 'a list = <fun>*)
(*remove 3 [2;6;3;4;3];;
- : int list = [2; 6; 4; 3]*)
(*remove 3 [1;2;4];;    
- : int list = [1; 2; 4]*)

(*remove_all: 'a -> 'a list -> 'a list, elimina todas las apariciones de un valor en una lista*)
let rec remove_all v =function
		[]->[]
		|h::t-> if v=h then remove_all v t
				else h::remove_all v t;; (*val remove_all : 'a -> 'a list -> 'a list = <fun>*)
(*remove_all 3 [2;6;3;4;3];;
- : int list = [2; 6; 4]*)

(*ldif: 'a list -> 'a list -> 'a list, ldif l1 l2 elimine de l1 todas las apariciones de todos aquellos valores que aparezcan en l2*)
let rec remove_all v =function
		[]->[]
		|h::t-> if v=h then remove_all v t
				else h::remove_all v t;;

let rec ldif l1 l2=match l2 with
				[]->l1
				|h::t->ldif(remove_all h l1) t;; (*val ldif : 'a list -> 'a list -> 'a list = <fun>*)
(*ldif [1;2;3;2;4] [2;3;3;5];;
- : int list = [1; 4]*)

(*lprod: 'a list -> 'b list -> ('a * 'b) list, lprod l1 l2 calcule producto cartesiano de l1 l2*)
let rec map f = function
     [] -> []
     |h::t -> f h::map f t;;

let rec lprod l1 l2=match l1 with
				[]->[]
				|h::t->(map(function x -> (h,x))l2) @ (lprod t l2);; (*val lprod : 'a list -> 'b list -> ('a * 'b) list = <fun>*)
(*lprod [1;3;1;2] ['a';'b'];;
- : (int * char) list =
[(1, 'a'); (1, 'b'); (3, 'a'); (3, 'b'); (1, 'a'); (1, 'b'); (2, 'a');
 (2, 'b')]*)
 
 (*divide: 'a list -> 'a list * 'a list, devide l y devuelve un par de listas (l1,l2), l1 contiene los elementos de l que ocupan posicion impar y l2 los que ocupan posicion par. Deben mantener mismo orden relativo que tienen en la lista original*)
 let divide l  =
    match l with
    []->([],[])
    |h::t-> let rec aux l acc l1 l2 =
        match l with
        []->(List.rev(l2),List.rev(l1))
        |h::t-> if (acc mod 2)==0 then aux t (acc+1) (h::l1) l2
                else aux t (acc+1) l1 (h::l2)
        in aux l 1 [] [] ;; (*val divide : 'a list -> 'a list * 'a list = <fun>*)
(*divide ['a';'e';'i';'o';'u'];;
- : char list * char list = (['a'; 'i'; 'u'], ['e'; 'o'])*)
