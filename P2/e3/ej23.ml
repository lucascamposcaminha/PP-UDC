*let e1 = 
	let pi = 2. *. asin 1. in pi *. (pi +. 1.);;*
let e1 = (function pi -> pi *. (pi +. 1.)) (2. *. asin 1.);; *val e1 : float = 13.0111970546791511*

*let e2 =
	let lg2 = log 2. in
	let log2 = function x -> log x /. lg2
	in log2 (float (1024 * 1024));;*
let e2 = (function (lg2)->((function (log2) -> (log2 (float (1024 * 1024))))(function x -> log x /. lg2)))(log 2.);;*val e2 : float = 20.*

*let e3 =
	let pi_2 = 4. *. asin 1. in
	function r -> pi_2 *. r;;*
let e3 = function pi_2 -> (function r -> pi_2 *. r)(4. *. asin 1.);; *val e3 : float -> float = <fun>*

*let e4 =
	let sqr = function x -> x *. x in
	let pi = 2. *. asin 1. in
	function r -> pi *. sqr r;;*
let e4 = (function (sqr) -> ((function (pi) -> (function (r) -> (pi *. sqr r)))(2. *. asin 1.)))(function x -> x *. x);;*val e4 : float -> float = <fun>*
