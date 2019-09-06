datatype mytype = TwoInts of int*int
                | Str of string
                | Pizza

val a = Str "hi"
val b = Str 
val c = Pizza 
val d = TwoInts(1+2, 3+4) 
val e = a

fun f x = 
    case x of 
        Pizza => 3
      | Str s => 8
      | TwoInts(i1, i2) => i1 + i2