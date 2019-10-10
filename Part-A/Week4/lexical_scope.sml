(* x maps to 1 *)
val x = 1
(* f maps to a function that adds 1 to its argument *)
fun f y = x + y
(* x maps to 2 *)
val x = 2
(* y maps to 3 *)
val y = 3
(* call the function defined on line 4 with 5 *)
(* z maps to 6 *)
val z = f(x+y)