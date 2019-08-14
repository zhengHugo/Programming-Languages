fun remove_duplicate(darr : int list) = 
    if null darr
    then []
    else 
    let 
        fun is_unique(key : int, arr : int list) = 
            if null arr
            then true
            else
                if key = hd arr
                then false
                else is_unique(key, tl arr)
        val tl_ans = remove_duplicate(tl darr)
    in
        if is_unique(hd darr, tl_ans)
        then hd darr:: tl_ans
        else tl_ans
    end

val arr= [1,1,3,2,4,4,2,3]
val t = remove_duplicate(arr)