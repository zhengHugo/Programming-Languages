fun sorted3 x y z = z >= y andalso y >= x

fun fold f acc xs = 
    case xs of
        [] => acc
      | x::xs' => fold f (f(acc, x)) xs' 

val is_nonnagative = (sorted3 0) 0

val sum = fold (fn (x, y) => x + y) 0

fun range i j = if i > j then [] else i::range (i + 1) j

val countup = range 1

fun exists predicate xs = 
    case xs of 
        [] => false
      | x::xs' => predicate x orelse exists predicate xs'

val hasZero = exists (fn x => x = 0)
