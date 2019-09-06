datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int
datatype id = StudentNum of int
            | Name of string * (string option) * string

datatype exp = Constant of int
             | Negate   of exp
             | Add      of exp * exp
             | Multiply of exp * exp

fun eval e = 
    case e of 
        Constant i          => i
      | Negate e2           => ~ (eval e2)
      | Add(e1, e2)         => (eval e1) + (eval e2)
      | Multiply(e1, e2)    => (eval e1) * (eval e2)


fun max_constant e = 
    case e of 
        Constant i  =>  i 
      | Negate e2   =>  max_constant e2 
      | Add(e1, e2) =>  
        Int.max(max_constant e1, max_constant e2)
      | Multiply(e1, e2) => max_constant(Add(e1,e2))

fun sum_list xs =
    case xs of 
        [] => 0
      | x::xs' => x + sum_list xs'

fun sum_triple (x, y, z) = 
    x + y + z

fun full_name {first = x, middle = y, last = z} = 
    x ^ " " ^ y ^ " " ^ z
  
exception ListLengthMismatch

fun zip list_tuple = 
    case list_tuple of 
        ([], [], []) => []
      | (hd1::tl1, hd2::tl2, hd3::tl3) 
          => (hd1, hd2, hd3)::zip (tl1, tl2, tl3)
      | _ => raise ListLengthMismatch

fun unzip tuple_list = 
    case tuple_list of 
        [] => ([], [], [])
      | (x, y, z) :: tl 
        => 
          let (tl1, tl2, tl3) = unzip(tl)
          in (x::tl1, y::tl2, z::tl3)
          end

val t1 = (zip([1,2,3], [4,5,6], [7,8,9]) 
        = [(1,4,7), (2,5,8), (3,6,9)])
val t2 = (unzip([(1,4,7), (2,5,8), (3,6,9)]) 
        = ([1,2,3], [4,5,6], [7,8,9]))