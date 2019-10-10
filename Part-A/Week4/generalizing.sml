fun double_or_triple f =
    if f 7
    then fn x => 2*x
    else fn x => 3*x

val double = double_or_triple (fn x => x-3=4)
val triple = double_or_triple (fn x => x=42)
val nine = triple 3