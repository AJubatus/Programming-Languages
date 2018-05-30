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
fun only_capitals x =
  List.filter (fn y => Char.isUpper(String.sub (y, 0))) x

fun longest_string1 ls =
  List.foldl (fn (x,y)
              => if String.size x > String.size y then x else y)
              "" ls

fun longest_string2 ls =
  List.foldl (fn (x, y)
              => if String.size x >= String.size y then x else y)
              "" ls

fun longest_string_helper f ls =
  List.foldl (fn (x, y)
              => if f(String.size x, String.size y)
                 then x
                 else y)
              "" ls

val longest_string3 = longest_string_helper 
                       (fn (x, y) => x > y)

val longest_string4 = longest_string_helper 
                       (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
  case xs of
    []     => raise NoAnswer
  | x::xs' => case f x of
                   SOME v => v
                 | NONE   => first_answer f xs'

fun all_answers f xs =
  let
    fun helper (f, acc, xs) =
      case xs of
        []     => SOME acc
      | x::xs' => case f x of
                   NONE   => NONE
                 | SOME v => helper(f, acc @ v, xs')
  in
    helper (f, [], xs)
  end

fun count_wildcards p =
  g (fn _ => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p =
  g (fn _ => 1) (fn x => String.size x) p

fun count_some_var (s, p) =
  g (fn _ => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
  let
    fun variable_strings p =
      case p of
        Variable x        => [x]                                                        
      | TupleP ps         => List.foldl (fn (x, acc) =>  acc @ variable_strings(x)) [] ps  
      | ConstructorP(_,p) => variable_strings(p)
      | _                 => []
    fun repeats sl =
      case sl of
        []    => true
      | x::xs => if List.exists (fn s => x = s) xs
                 then false
                 else repeats(xs)
  in
    repeats(variable_strings(p))
  end

fun match (v, p) =
  case (p, v) of
       (Wildcard, _)                              => SOME []
     | (Variable s, _)                            => SOME [(s, v)]
     | (UnitP, Unit)                              => SOME []
     | (ConstP cp, Const cv)                      => if cp = cv then SOME [] else NONE
     | (TupleP ps, Tuple vs)                      => if List.length ps = List.length vs
                                                     then all_answers match (ListPair.zip(vs, ps))
                                                     else NONE
     | (ConstructorP(s1, pp), Constructor(s2,pv)) => if s1 = s2 then match(pv, pp) else NONE
     | _                                          => NONE

fun first_match v ps =
  (SOME (first_answer (fn p => match(v, p)) ps) ) handle NoAnswer => NONE
