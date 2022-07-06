*Un valor u de tipo int a partir de una expresión que contenga, al menos, 4 operadores infijos.*
 let u = 1 + 2 - 3 + 4 + 5;;
 
*Un valor v de tipo float a partir de una expresión que incluya una función predefinida*
 let v = float_of_int 8;;
 
 *Un valor w de tipo char a partir de una expresión que incluya una sub-expresión de tipo int*
 let w = char_of_int 75;;
 
 *Un valor x de tipo bool a partir de una expresión que incluya una o más funciones u operadores.*
 let x = function "PP" -> true | "pp" -> true | _ -> false;;
 
 *Un valor y de tipo string a partir de una expresión que contenga una frase if_then_else*
 let y = if 3 = 4 then "0" else "4";;
