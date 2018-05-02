fun is_older (first: int * int * int, second: int * int * int) =
    if #1 second > #1 first
    then true
    else if (#1 second = #1 first) andalso (#2 second > #2 first)
    then true
    else (#1 second = #1 first) andalso (#2 second = #2 first) andalso (#3 second > #3 first)

fun number_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month (tl dates, month)
    else number_in_month (tl dates, month)

fun number_in_months(dates: (int * int * int) list, months: int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)

fun dates_in_month(dates: (int * int * int) list, month: int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months(dates: (int * int * int) list, months: int list) =
    if null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

fun get_nth (strings: string list, n: int) =
    if n = 1
        then hd strings
    else
        get_nth(tl strings, (n - 1))

fun date_to_string(date: int * int * int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in 
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum: int, numbers: int list) =
    if null numbers
    then 0
    else
        let fun helper_sum (sum: int, numbers: int list) =
                if hd numbers >= sum
                    then 0
                else  
                    1 + helper_sum(sum - hd numbers, tl numbers)
        in
            helper_sum(sum, numbers)
        end

fun what_month(day: int) =
    let
        val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in 
        1 + number_before_reaching_sum (day, days_in_months) 
    end

fun month_range (day1: int, day2: int) =
    if day1 > day2
        then []
    else
        what_month day1 :: month_range (day1 + 1, day2)

fun oldest (dates: (int * int * int) list) =
    if null dates
    then NONE
    else
        let fun oldest_from (dates: (int * int * int) list, oldest: (int * int * int)) =
            if null dates 
            then SOME oldest
            else
                if is_older(hd dates, oldest)
                then oldest_from(tl dates, hd dates)
                else oldest_from(tl dates, oldest)
        in
            oldest_from(dates, hd dates)
        end