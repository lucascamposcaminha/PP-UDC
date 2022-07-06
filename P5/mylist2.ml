(*Funciones: hd, tl, nth, init, map2, find y combine, considerando casos de error de ejecucion*)

(*hd devuelve el primer elemento de la lista*)
let hd = function
     [] -> raise (Failure "hd")
     | h::_ -> h;; (*val hd : 'a list -> 'a = <fun>*)
     
(*tl devuelve la lista que le pongamos quitando el primer elemento de esta *)
let tl = function
     [] -> raise (Failure "tl")
     |_::t -> t;; (*val tl : 'a list -> 'a list = <fun>*)
    
(*nth devuelve x elemento de la lista según la posición que le indiquemos*)
let nth l n =
  if n < 0 then raise (Failure "nth") else
  let rec nth_aux l n = match l with
    | [] -> raise (Failure "nth")
    | a::l -> if n = 0 then a else nth_aux l (n-1)
  in nth_aux l n;; (*val nth : 'a list -> int -> 'a = <fun>*)
    
(*init devuelve una lista del tipo que le mandemos*)
let init l1 f =
    let rec aux l x= 
        match l with
        |[]-> aux (f(x)::[]) (x-1)
        |_::_-> if x <0 then l else aux (f(x)::l) (x -1)
         in aux [] (l1-1);; (*val init : int -> (int -> 'a) -> 'a list = <fun>*)

(*map2 devuelve lo mismo que map, pero devuelve Invalid_argument si las dos listas tienen longitudes diferentes y no emplea recursion de cola*)
let rec map2 f l1 l2=match(l1,l2) with
         ([],[])->[]
         |(h1::t1,h2::t2)->f h1 h2 :: map2 f t1 t2
         |(_,_)-> raise (Failure "map2");; (*val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list = <fun>*)
    
(*find devuelve el primer elemento de la lista que cumpla la condicion x*)
let rec find p=function
            []->raise Not_found
            |(h::t)->if p h then h else find p t;; (*val find : ('a -> bool) -> 'a list -> 'a = <fun>*)
            
(*combine transforma un par de listas en una lista de parejas, si las listas tienen longitudes diferentes lanza Invalid_argument*)
let rec combine l1 l2=match(l1,l2) with
	([],[])->[]
	|(h1::l1,h2::l2)->(h1,h2)::combine l1 l2
	|(_,_)->raise(Failure "combine");; (*val combine : 'a list -> 'b list -> ('a * 'b) list = <fun>*)
      

(*Funciones: length, compare_lengths, nth, rev, init, rev_append, rev_map, fold_left, find, for_all, exists, mem, filter, find_all, partition, son recursivas terminales*)

(*length devuelve el tamaño de la lista*)
let length l =
     let rec aux x = function
     [] -> x
     |h::t -> aux (x+1) t
     in aux 0 l;; (*val length : 'a list -> int = <fun>*)
     
(*compare_lengths compara longitudes de dos listas*)
let rec compare_lengths l1 l2 = match (l1,l2)
       with [],[] -> 0                         
       |[],_ -> 2                              
       |_,[] -> 1                              
       |_::t1,_::t2 -> compare_lengths t1 t2;; (*val compare_lengths : 'a list -> 'b list -> int = <fun>*)

(*nth devuelve x elemento de la lista según la posición que le indiquemos*)
let nth l n =
  if n < 0 then raise (Failure "nth") else
  let rec nth_aux l n =
    match l with
    | [] -> raise (Failure "nth")
    | a::l -> if n = 0 then a else nth_aux l (n-1)
  in nth_aux l n;; (*val nth : 'a list -> int -> 'a = <fun>*)
     
(*rev devuelve la lista en su orden inverso*)
let rev l =
     let rec aux l a = match l with
     [] -> a
     | h::t -> aux t (h::a)
     in aux l[];; (*val rev : 'a list -> 'a list = <fun>*) 

(*init devuelve una lista del tipo que le mandemos*)
let init l1 f =
    let rec aux l x= 
        match l with
        |[]-> aux (f(x)::[]) (x-1)
        |_::_-> if x <0 then l else aux (f(x)::l) (x -1)
         in aux [] (l1-1);; (*val init : int -> (int -> 'a) -> 'a list = <fun>*)
    	
(*rev_append concatena dos listas pero los elementos de la primera lista tienen orden inverso*)
let rec rev_append l1 l2 = match l1 with
    	[] -> l2
  	| a::l -> rev_append l (a :: l2);; (*val rev_append : 'a list -> 'a list -> 'a list = <fun>*)

(*rev_map devuelve lo mismo que rev juntado con map, pero emplea recursion de cola y es mas eficiente*)
let rev_map f l =
  let rec aux x = function
    | [] -> x
    | a::l -> aux (f a :: x) l
  in aux [] l;; (*val rev_map : ('a -> 'b) -> 'a list -> 'b list = <fun>*)

(*fold_left corre a traves de la lista (empieza por la izquierda) *)
let rec fold_left f e = function
     [] -> e
     |h::t -> fold_left f (f e h) t;; (*val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>*)

(*find devuelve el primer elemento de la lista que cumpla la condicion x*)
let rec find p=function
            []->raise (Failure "find")
            |(h::t)->if p h then h else find p t;; (*val find : ('a -> bool) -> 'a list -> 'a = <fun>*)

(*for_all si todos los elementos de la lista cumplen x condicion, devolvera true o false (bool)*)
let rec for_all p = function
     [] -> true
     |h::t -> (p h) && (for_all p t);; (*val for_all : ('a -> bool) -> 'a list -> bool = <fun>*)

(*exist si existe algun elemento de la lista que cumpla x condicion, devolvera true o false (bool)*)
let rec exists p=function
                  []->false
                  |(h::t)->p h || exists p t;; (*val exists : ('a -> bool) -> 'a list -> bool = <fun>*)

(*mem devuelve true o false (bool) dependiendo si esta x elemento en una lista*)
let rec mem x = function        
       [] -> false                
       | h::t -> if (x = h) then true
			else (mem x t);; (*val mem : 'a -> 'a list -> bool = <fun>*)

(*filter devuelve los elementos que cumplan x condicion sobre una lista*)
let filter p l =
	let rec aux l1 l2 = function
		[] -> rev l1
		| h::t -> if (p h) then aux (h::l1) l2 t
			else aux l1 l2 t
	in aux [] [] l;; (*val filter : ('a -> bool) -> 'a list -> 'a list = <fun>*)

(*find_all devuelve todos los elementos de la lista que cumplan x condicion*)
let find_all p l =
	let rec aux l1 l2 = function
		[] -> rev l1
		| h::t -> if (p h) then aux (h::l1) l2 t
			else aux l1 l2 t
	in aux [] [] l;; (*val find_all : ('a -> bool) -> 'a list -> 'a list = <fun>*)

(*partition retorna dos listas, la primera que cumpla x condicion y la segunda que no cumpla x condicion*)
let partition p l =
	let rec aux l1 l2 = function
		[] -> (rev l1, rev l2)
		| h::t -> if (p h) then aux (h::l1) l2 t
			else aux l1 (h::l2) t
	in aux [] [] l;; (*val partition : ('a -> bool) -> 'a list -> 'a list * 'a list = <fun>*)
	
(*Funciones que no se cambiaron: *)
(*fold_right corre a traves de la lista (empieza por la derecha) *)
let rec fold_right f l e = match l with
       [] -> e
       | h::t -> f h(fold_right f t e);; (*val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun>*)
       
(*map devuelve una lista en el formato que le digamos*)
let rec map f = function
     [] -> []
     |h::t -> f h::map f t;; (*val map : ('a -> 'b) -> 'a list -> 'b list = <fun>*)
     
(*flatten hace lo mismo que concat*)
let rec flatten = function
    [] -> []
  | h::t -> h @ flatten t;; (*val flatten : 'a list list -> 'a list = <fun>*)
  
(*concat concatena listas, da igual si son de diferente tamaño*)
let rec concat = function
    [] -> []
  | h::t -> h @ concat t;; (*val concat : 'a list list -> 'a list = <fun>*)
  
(*split transforma una lista de parejas en un par de listas*)
let rec split = function 
	[] -> ([],[])
	| (a,b)::t -> let (l1,l2) = split t in
	(a::l1, b::l2);; (*val split : ('a * 'b) list -> 'a list * 'b list = <fun>*)
	
(*append concatena 2 listas*)
let rec append l1 l2 = match l1 with
       [] -> l2
       | h::t -> h::append t l2;;(*val append : 'a list -> 'a list -> 'a list = <fun>*)
