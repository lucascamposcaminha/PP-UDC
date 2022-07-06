*curry : (('a * 'b) -> 'c -> ('a ->('b -> 'c)) == let curry f x y = f (x,y)*
*uncurry : ('a -> ('b -> 'c)) -> (('a * 'b) -> 'c) == let uncurry f (x,y) = f x y*

uncurry (+);; *-:int * int -> int = <fun>*
let sum = (uncurry (+));; *val sum : int * int -> int = <fun>*
sum 1;; *Error de compilacion de tipo*
sum (2,1);; *-:int = 3*
let g = curry (function p -> 2 * fst p + 3 * snd p);; *val g: int -> int -> int = <fun>*
g (2,5);; *Error de compilacion de tipo*
let h = g 2;; *val h : int -> int = <fun>*
h 1, h 2, h 3;; *-:int * int * int = (7,10,13)*
