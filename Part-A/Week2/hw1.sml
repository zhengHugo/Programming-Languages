(* True if the first date comes before the second *)
fun is_older (d1 : (int * int * int), d2 : (int * int * int)) = 
    if #1 d1 < #1 d2
    then true
    else 
        if #1 d1 > #1 d2 
        then false
        else
            if #2 d1 < #2 d2
            then true
            else 
                if #2 d1 > #2 d2
                then false
                else #3 d1 < #3 d2
                
(* How many dates in the list are in the given month *)
fun number_in_month (dates : (int*int*int) list, month : int) = 
    if null dates
    then 0
    else
        let val tl_number = number_in_month(tl dates, month)
        in 
            if #2 (hd dates) = month
            then 1 + tl_number
            else tl_number
        end

fun number_in_months (dates : (int*int*int) list, months : int list) = 
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) = 
    if null dates
    then []
    else
        let val tl_dates = dates_in_month(tl dates, month)
        in 
            if #2 (hd dates) = month
            then hd dates :: tl_dates
            else tl_dates
        end

fun dates_in_months(dates : (int*int*int) list, months : int list) = 
    if null months 
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strs : string list, n : int) = 
    if n = 1
    then hd strs
    else get_nth(tl strs, n-1)

fun date_to_string(date : (int*int*int)) = 
    let 
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum : int, arr : int list) = 
    if hd arr >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - hd arr, tl arr)

fun what_month(dayOfYear : int) = 
    let val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(dayOfYear, month_days) + 1
    end

fun month_range(day1 : int, day2 : int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

fun oldest(dates : (int*int*int) list) = 
    if null dates
    then NONE 
    else 
        let val tl_ans = oldest(tl dates)
        in 
            if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
            then tl_ans
            else SOME(hd dates)
        end

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) = 
    let 
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
    in
            number_in_months(dates, remove_duplicate(months))
    end 

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
    let 
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
    in
            dates_in_months(dates, remove_duplicate(months))
    end 


fun reasonable_date(date : (int*int*int)) = 
    let 
        val year = #1 date
        val month = #2 date
        val day = #3 date
    in 
        if year > 0 andalso 0 < month andalso month < 13
        then 
            let
                val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
                fun is_leapyear(year : int) = 
                    if year mod 400 = 0
                    then true
                    else year mod 4 = 0 andalso not (year mod 100 = 0)
                fun get_nth_int(arr : int list, n : int) = 
                    if n = 1
                    then hd arr
                    else get_nth_int(tl arr, n-1)
            in
                if is_leapyear(year) andalso month = 2
                then 0 < day andalso day < 30
                else 0 < day andalso day < get_nth_int(month_days, month)
            end
        else false
    end

