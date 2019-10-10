val x = 1

fun f y = 
    let 
        val x = y + 1
    in 
        fn z => x + y + z
        (* take z and return 2y+1+z *)
    end

val x = 3 (* irrelevant *)

(* return a function that adds 9 to its argument  *)
val g = f 4 

val y = 5 (* irrelevant *)
val z = g 6
