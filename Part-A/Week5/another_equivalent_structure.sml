signature RATIONAL_B =
sig
  type rational 
  exception BadFrac
  val make_frac : int * int -> rational
  val add : rational * rational -> rational
  val toString : rational -> string
end

structure Rational3 :> RATIONAL_B =
struct
    type rational = int * int 
    exception BadFrac

    fun make_frac(x, y) = 
        if y = 0
        then raise BadFrac
        else if y < 0
        then (~x, ~y)
        else (x, y)

    fun add ((a,b), (c,d)) = (a*d + c*b, b*d)

    fun toString (x, y) = 
        if x = 0
        then "0"
        else 
            let
                fun gcd (x, y) = 
                    if x = y
                    then x
                    else if x < y
                    then gcd(x, y-x)
                    else gcd(y, x)
                val d = gcd(abs x, y)
                val num = x div d
                val denom = y div d 
            in
                Int.toString num ^ ( 
                    if denom = 1 
                    then "" 
                    else "/" ^ (Int.toString denom)
                )
            end
        fun Whole i = (i, 1)
end