(*Funciones: hd, tl, length, compare_lengths, nth y append, del modulo List*)
(*No utilizar ese modulo ni funcion @ del modulo Pervasives*)

(*hd devuelve el primer elemento de la lista, si esta es vacia devuelve error*)
let hd = function
     [] -> raise (Failure "hd")
     | h::_ -> h;; (*val hd : 'a list -> 'a = <fun>*)
     
(*tl devuelve la lista que le pongamos quitando el primer elemento de esta, si esta es vacia devuelve error*)
let tl = function
     [] -> raise (Failure "tl")
     |_::t -> t;; (*val tl : 'a list -> 'a list = <fun>*)
    
(*length devuelve el tamaño de la lista, hay peligro de agotamiento de la pila porque hay operaciones pendientes que a cada iteracion puede llenar la pila*)
let rec length = function
     [] -> 0
     |_::t -> 1 + length t;; (*val length : 'a list -> int = <fun>*)
     
(*compare_lengths compara longitudes de dos listas, 0 implica que las dos listas son iguales de tamaño, 2 implica que la lista l2 tiene un tamaño mayor que l1 y 1 implica que l1 tiene un tamaño mayor que l2*)
let rec compare_lengths l1 l2 = match (l1,l2)
       with [],[] -> 0                         
       |[],_ -> 2                              
       |_,[] -> 1                              
       |_::t1,_::t2 -> compare_lengths t1 t2;; (*val compare_lengths : 'a list -> 'b list -> int = <fun>*)

(*nth devuelve x elemento de la lista según la posición que le indiquemos, devuelve error si p es un numero negativo*)
let rec nth l p = match l,p with
     [],_ -> raise(Failure "nth")
     |h::_,0 -> h
     |_::t,p -> if p>0 then nth t (p-1)
     else raise(Failure "nth");; (*val nth : 'a list -> int -> 'a = <fun>*)

(*append concatena 2 listas*)
let rec append l1 l2 = match l1 with
       [] -> l2
       | h::t -> h::append t l2;;(*val append : 'a list -> 'a list -> 'a list = <fun>*)

(*Funciones find, for_all, exist, mem, filter, find_all, partition, split, combine, del modulo List sin utilizar este*)
(*find devuelve el primer elemento de la lista que cumpla la condicion x*)
let rec find p=function
            []->raise Not_found
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
       | h::t -> x = h || mem x t;; (*val mem : 'a -> 'a list -> bool = <fun>*)

(*filter devuelve los elementos que cumplan x condicion sobre una lista*)
let rec filter f =function
	[]-> []
	|h::t ->if f h then h::filter f t
	else filter f t;; (*val filter : ('a -> bool) -> 'a list -> 'a list = <fun>*)

(*find_all devuelve todos los elementos de la lista que cumplan x condicion*)
let rec find_all f =function
	[]-> []
	|h::t ->if f h then h::find_all f t
	else find_all f t;; (*val find_all : ('a -> bool) -> 'a list -> 'a list = <fun>*)

(*partition retorna dos listas, la primera que cumpla x condicion y la segunda que no cumpla x condicion*)
let rec partition f= function
	[] ->([],[])
	|h::t->let (l1,l2)=partition f t in
			if f h then (h::l1,l2)
			else (l1,h::l2);; (*val partition : ('a -> bool) -> 'a list -> 'a list * 'a list = <fun>*)

(*split transforma una lista de parejas en un par de listas*)
let rec split = function 
	[] -> ([],[])
	| (a,b)::t -> let (l1,l2) = split t in
	(a::l1, b::l2);; (*val split : ('a * 'b) list -> 'a list * 'b list = <fun>*)

(*combine transforma un par de listas en una lista de parejas, si las listas tienen longitudes diferentes lanza Invalid_argument*)
let rec combine l1 l2=match(l1,l2) with
	([],[])->[]
	|(h1::l1,h2::l2)->(h1,h2)::combine l1 l2
	|(_,_)->raise(Failure "combine");; (*val combine : 'a list -> 'b list -> ('a * 'b) list = <fun>*)

(*Funciones init, rev, rev_append, concat, flatten, map, rev_map, map2, fold_left, fold_right, del moulo List sin utilizar este*)
(*init devuelve una lista del tipo que le mandemos*)
let init l1 f =
    let rec aux l x= 
        match l with
        |[]-> aux (f(x)::[]) (x-1)
        |_::_-> if x <0 then l else aux (f(x)::l) (x -1)
         in aux [] (l1-1);; (*val init : int -> (int -> 'a) -> 'a list = <fun>*)

(*rev devuelve la lista en su orden inverso*)
let rev l =
	let rec aux l x = match l with
	  [] -> x
	| h::t -> aux t (h::x)
  	in aux l [];;  (*val rev : 'a list -> 'a list = <fun>*) 

(*rev_append concatena dos listas pero los elementos de la primera lista tienen orden inverso*)
let rec rev_append l1 l2 = match l1 with
     [] -> l2
     |h::t -> rev_append t(h::l2);; (*val rev_append : 'a list -> 'a list -> 'a list = <fun>*)

(*concat concatena listas, da igual si son de diferente tamaño*)
let rec concat = function
    [] -> []
  | h::t -> h @ concat t;; (*val concat : 'a list list -> 'a list = <fun>*)

(*flatten hace lo mismo que concat*)
let rec flatten = function
    [] -> []
  | h::t -> h @ flatten t;; (*val flatten : 'a list list -> 'a list = <fun>*)

(*map devuelve una lista en el formato que le digamos*)
let rec map f = function
     [] -> []
     |h::t -> f h::map f t;; (*val map : ('a -> 'b) -> 'a list -> 'b list = <fun>*)

(*rev_map devuelve lo mismo que rev juntado con map, pero emplea recursion de cola y es mas eficiente*)
let rev_map f l =
  let rec rmap_f aux = function
    | [] -> aux
    | a::l -> rmap_f (f a :: aux) l
  in
  rmap_f [] l;; (*val rev_map : ('a -> 'b) -> 'a list -> 'b list = <fun>*)

(*map2 devuelve lo mismo que map, pero devuelve Invalid_argument si las dos listas tienen longitudes diferentes y no emplea recursion de cola*)
let rec map2 f l1 l2=match(l1,l2) with
         ([],[])->[]
         |(h1::t1,h2::t2)->f h1 h2 :: map2 f t1 t2
         |(_,_)-> raise (Failure "map2");; (*val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list = <fun>*)

(*fold_left corre a traves de la lista (empieza por la izquierda) *)
let rec fold_left f e = function
     [] -> e
     |h::t -> fold_left f (f e h) t;; (*val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>*)

(*fold_right corre a traves de la lista (empieza por la derecha) *)
let rec fold_right f l e = match l with
       [] -> e
       | h::t -> f h(fold_right f t e);; (*val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun>*)
