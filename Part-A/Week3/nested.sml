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
          let val (tl1, tl2, tl3) = unzip(tl)
          in (x::tl1, y::tl2, z::tl3)
          end
      | _ => raise ListLengthMismatch


val t1 = (zip([1,2,3], [4,5,6], [7,8,9]) 
        = [(1,4,7), (2,5,8), (3,6,9)])
val t2 = (unzip([(1,4,7), (2,5,8), (3,6,9)]) 
        = ([1,2,3], [4,5,6], [7,8,9]))
    

fun nondecreasing xs = (* int list -> bool *)
    case xs of 
        [] => true
      | x::[] => true
      | x::y::ys => x <= y andalso nondecreasing y::ys 

datatype sgn = P | N | Z 

fun mult_sgn
