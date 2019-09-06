(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(s, sl) = 
    case sl of
        [] => NONE
      | x::xs => 
        if same_string(s,x)
        then SOME(xs)
        else 
            case all_except_option(s, xs) of
                NONE => NONE
              | SOME y => SOME(x::y)

fun get_substitutions1(sll, s) = 
    case sll of
        [] => []
      | x::xs =>
            case all_except_option(s, x) of
                NONE => get_substitutions1(xs, s)
              | SOME y => y @ get_substitutions1(xs, s)

fun get_substitutions2(sll, s) = 
    let fun aux(sll, s, acc) = 
        case sll of
            [] => acc
          | x::xs => 
            case all_except_option(s,x) of
                NONE => aux(xs, s, acc)
              | SOME(ys) => aux(xs, s, acc @ ys)
    in aux(sll, s, [])
    end

fun similar_names(sll, {first=x1, middle=x2, last=x3}) = 
    let fun helper(fl, middleName, lastName) =
        case fl of
            [] => []
          | f::fs => {first=f, middle=middleName, last=lastName}::helper(fs, middleName, lastName)
    in
        {first=x1, middle=x2, last=x3}::helper(get_substitutions2(sll, x1), x2, x3)
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(suit, rank) =
    case suit of
        (Spades | Clubs) => Black
      | _ => Red


fun card_value(suit, rank) =
    case rank of 
        Num x => x
      | Ace => 11
      | _ => 10

fun remove_card(cs, c, e) =
    case cs of 
        [] => raise e 
      | x::xs => 
            if x = c 
            then xs
            else x::remove_card(xs, c, e)

fun all_same_color(cs) = 
    case cs of
        [] => true
      | x::[] => true
      | x::y::ys =>
             card_color(x) = card_color(y) andalso all_same_color(y::ys)

fun sum_cards(cs) = 
    let fun aux(cs, acc) = 
        case cs of
            [] => acc
          | x::xs => aux(xs, acc + card_value(x))
    in aux(cs, 0)
    end

fun score(cs, goal) =
    let 
        val sum = sum_cards(cs)
    in
        let val pre_score = 
            if sum > goal
            then 3 * (sum - goal)
            else (goal - sum)
        in
            if all_same_color(cs)
            then pre_score div 2
            else pre_score 
        end
    end

fun officiate(cds, ms, goal) =
    let fun aux(hds, cds, ms, goal) =
        case ms of
            [] => score(hds, goal)
          | m::ms' => 
                case m of
                    Discard(c) => 
                        aux(remove_card(hds, c, IllegalMove), cds, ms', goal)
                  | Draw =>
                        case cds of 
                            [] => score(hds, goal)
                          | c::cs => 
                                if sum_cards(c::hds) > goal
                                then score(c::hds, goal)
                                else aux(c::hds, cs, ms', goal)
    in aux([], cds, ms, goal)
    end
