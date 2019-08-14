(* This is a comment. This is our first program. *)
val x = 34;
(* static environment: x : int *)
(* dynamic environment: x --> 34 *)
val y = 17;
(* dynamic environment: x --> 34 *)
val z = (x + y) + (y + 2);

val abs_of_z = if z < 0 then 0 - z else z;
(* dynamic environment: ..., abs_of_z --> 70 *)

val abs_of_z_simpler = abs z;