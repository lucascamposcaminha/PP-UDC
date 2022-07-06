(*Ejercicio 4*)
let is_prm n =
	let rec not_divisible_from d =
	d * d > n || (n mod d <> 0 && not_divisible_from (d+1)) in
	n > 1 && not_divisible_from 2;;
	
let goldbach n =
    let rec aux d =
      if is_prm d && is_prm (n - d) then (d, n-d)
      else aux (d+1) in
    aux 2;;
(*goldbach 28;;
- : int * int = (5, 23)*)
