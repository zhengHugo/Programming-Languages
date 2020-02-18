(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
         | Variable of string
         | UnitP
         | ConstP of int
         | TupleP of pattern list
         | ConstructorP of string * pattern

datatype valu = Const of int
          | Unit
          | Tuple of valu list
          | Constructor of string * valu

fun g f1 f2 p =
    let 
    val r = g f1 f2 
    in
    case p of
        Wildcard          => f1 ()
      | Variable x        => f2 x
      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP(_,p) => r p
      | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string

(**** you can put all your code here ****)

(* Problem 1 *)
fun only_capitals(strs : string list) = 
    let 
    fun start_with_uppercase(str : string) = Char.isUpper(String.sub(str, 0)) 
    in 
    List.filter start_with_uppercase strs
    end

(* val x = only_capitals(["Abc", "1afds", "adfjs", "BJDK"]) *)
(* val y = only_capitals([]) *)

(* Problem 2 *)
fun longest_string1(strs : string list) = 
    if null strs 
    then "" 
    else List.foldl 
    (fn (s1, s2) => if String.size(s1) > String.size(s2) then s1 else s2)
    ("")
    (strs)


(* val x = longest_string1(["Abc", "1afds", "adfjs", "BJDK"])
val y = longest_string1([]) *)
 
(* Problem 3 *)
fun longest_string2(strs : string list) = 
    if null strs 
    then "" 
    else List.foldl 
    (fn (s1, s2) => if String.size(s1) >= String.size(s2) then s1 else s2)
    ("")
    (strs)


(* val x = longest_string2(["Abc", "1afds", "adfjs", "BJDK"])
val y = longest_string2([]) *)

(* Problem 4 *)
fun longest_string_helper(compare : int * int -> bool)(strs: string list) = 
    if null strs then ""
    else List.foldl(fn (s1, s2) 
        => if compare(String.size(s1), String.size(s2))
            then s1 else s2)("")(strs)


val longest_string3 = 
    fn (strs) => 
        longest_string_helper(fn (i, j)=> i > j)(strs)

val longest_string4 = 
    fn (strs) => 
        longest_string_helper(fn (i, j)=> i >= j)(strs)
    

(* val ls1 = longest_string1(["Abc", "1afds", "adfjs", "BJDK"])
val ls1_empty = longest_string1([])
val ls2 = longest_string2(["Abc", "1afds", "adfjs", "BJDK"])
val ls2_empty = longest_string2([])
val ls3 = longest_string3(["Abc", "1afds", "adfjs", "BJDK"])
val ls3_empty = longest_string3([])
val ls4 = longest_string4(["Abc", "1afds", "adfjs", "BJDK"])
val ls4_empty = longest_string4([]) *)


(* Problem 5 *)
val longest_capitalized = longest_string2 o only_capitals


(* val lc = longest_capitalized(["Abc", "1afds", "adfjs", "BJDK"])
val lc_empty = longest_capitalized([]) *)
