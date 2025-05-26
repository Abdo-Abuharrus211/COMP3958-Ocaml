(*Labeled argument and optional argument use the tilde, with labeled argument
use any order and relative order with non labeled*)
let f  ~a ~b c d  = (a,b,c,d);;

(*Optional arguments, when not specified or passed their type is None*)
let f a? b? c = (a,b,c);;
let f ?a:(Some 1) ?b:(Some 2) 3;;
(*Or just use tilde with optional too*)
let f ~a:1 ~b:2 3;;
(*Optional default arguments*)
let g ?(a=1) b? c = (a,b,c);;
g ~a:2 1;;

(*"JUST USE TILDE"*)

(*type annotation for a function*)
let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
	match l with
	| [] -> []
	| x :: xs -> f x :: map f xs
	
(*	Despite type annotation the compiler can infer type that's more restrictive.
	But the main use is to restrict they possible types inferred...*)

map (float_of_int) [1;2;3];;
map (( + ) 1) [1;2;3];;


(* String Module Fold functions. Synatax as follows: String.fold_left f acc str*)
String.fold_left (fun acc c -> if c == 'l' then acc + 1 else acc) 0 "Hello Melloo";;
String.fold_right (fun c acc -> if c == 'l' then acc + 1 else acc) "Hello Melloo" 0;;

(*Variant type also the Some type*)
type direction = North | South | West | East;;
(*Example function takes the variance and returns int*)
let f = function
	| North -> 1
	| South -> 2
	| West -> 3
	| East -> 4

(*Option type is aslo called 'maybe' type...*)
