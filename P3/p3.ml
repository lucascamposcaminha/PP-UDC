(*Ejercicio 1*)
let rec gcd a b =
       if b = 0 then a
       else gcd b (a mod b);;
(* val gcd : int -> int -> int = <fun> *)
(*gcd(a,0) = let a = 27;;
	    gcd 27 0;;*) (*- : int = 27*)
(*gcd(a,b) = let a = 27;;
	   let b = 67;;
	   gcd a b;;*) (*- : int = 1*)
	   
(*Ejercicio 2*)
let is_prm n =
	let rec not_divisible_from d =
	d * d > n || (n mod d <> 0 && not_divisible_from (d+1)) in
	n > 1 && not_divisible_from 2;; (* val is_prm : int -> bool = <fun> *)
(*Es una funcion de int a bool que introduciendo un numero por pantalla nos dice si este es primo o no*)
let is_prm2 n =
	let rec not_divisible_from d =
	(n mod d <> 0 && not_divisible_from (d+1)) || d * d > n in
	n > 1 && not_divisible_from 2;; (* val is_prm2 : int -> bool = <fun> *)
(*Es una funcion igual a la anterior*)
(*Mejor utilizar la primera, la segunda acumula operaciones de la recursion agotando la pila (es donde se alamcenan las operaciones recursivas, al agotar este es cuando salta el Stack over flow*)

(*is_prm 999983;;
- : bool = true
is_prm2 999983;;
Stack overflow during evaluation (looping recursion?).*)

(*Ejercicio 3*)
let capicua n = 
     let s = string_of_int n in
          let l = String.length s in
               let rec comp n = 
                    n = 0 || (s.[l-n] = s.[n-1] && comp (n-1)) in
                         comp (l/2);;
(*val capicua : int -> bool = <fun>*)
