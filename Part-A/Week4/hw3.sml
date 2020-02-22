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

(* Problem 6 *)
val rev_string = String.implode o List.rev o String.explode

(* val rs = rev_string("abcde")
val rs_empty = rev_string("") *)

(* Problem 7 *)
fun first_answer(func)(xs) = 
    case xs of 
        [] => raise NoAnswer
        | hd::tl => 
            let val ret = func(hd)
            in 
                if isSome ret then valOf(ret) else first_answer(func)(tl)
            end


(* Problem 8 *)
(* TODO: check sample answer *)
fun all_answers(func)(xs) =
    case xs of 
        [] => SOME([]) 
        | hd::tl => 
            let val hd_result = func(hd);
                val tl_result = all_answers(func)(tl);
            in
                if isSome(hd_result) andalso isSome(tl_result)
                then SOME(valOf(func(hd)) @ valOf(tl_result))
                else NONE
            end


(* fun func(x) = if x = 1 then NONE else SOME([x])
val test = all_answers(func)([2,3,4,6,5,3,3]) *)

(* Problem 9 *)
(* a *)
val count_wildcard = g (fn () => 1) (fn (x) => 0) 

(* b *)
val count_wild_and_variable_lengths =  g (fn () => 1) (String.size)

(* c *)
fun count_some_var(str, p) = 
    g (fn () => 0) (fn (x) => if x = str then 1 else 0) p

