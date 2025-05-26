(**Insert x into list l preserving the order
Required: that list l is sorted in ascending order **)

let rec insert x lst =
	match l with
	| [] -> [x]
	| y :: ys -> 
	if x <= y then x :: lst
	else y :: insert x list
	
	
(** Insertion Sort, returns a list srlst that's a copy of lst  in ascending order.
**)
let rec insertion_sort lst =
	match lst with
	| [] -> []
	| x ::  xs -> insert x (insertion_sort xs)
	
(** Returns the a list containing the first n elements of the list
- Precondition: n >= 0; if n < 0  then return an empty list
**) 
let rec take n lst =
	if n < 0 then []
	else match lst with
		| [] -> []
		| x :: xs -> x :: take (n -1) xs
(** taking but checking conditionsconditions
f is a function that takes an argument x and returns a boolean
**)
let rec take_while f l =
	match l with
	| [] -> []
	| x :: xs -> if f x then x :: take_while f xs (*if f x returns True...*)
			else []
			
	
(*Defining an anonymous is_even func; doesn't actually need the let,
can write anon func using the fun keyword wherever a function is expected*)
let f = fun x -> x mod 2 = 0;
let add' = fun x y -> x +y; (*this is actually `fun x -> (fun y -> x+y);;` *)

let (@@) f x =  fx;; (*try this in utop and see types*)

(*prefix operator*)
(+) 1 2;; (* Is the same as 1 + 2 but always remember the WHITE SPACES around the parenthesis.*)

(*The Option Type
utop # Some "Hello";;
- : string option = Some "Hello"
*)

(* Simple find function
	Where f is a predicate (a function that returns bool)
**)
let rec find f l = 
	match l with
	| [] -> None
	| x :: xs when f x -> Some x
	| _ xs -> find f xs

(*`find (fun x -> mod 2 = 0) [1; 3; 9]` should return int option = None*)

let default a x =
	match x with
	| None -> a
	| Some y -> y
(*`find (fun x -> mod 2 = 0) [1; 3; 9] |> default (-1)` should return int -1*)

let flip= fun x y = fun y x 

let rec map f l= function
	match l with
	| [] -> []
	| x :: xs -> f x :: map f xs
(* map (( - ) 2) [11;2;5]*)

let rec filter f l =
	match l with
	| [] -> []
	| x :: xs -> if f x then x :: filter f xs else filter f xs
	
let rec insert_v2 compare x l =
	match l with
	| [] -> [x]
	| y :: ys ->  if compare x y <= 0  then x :: l
			else y :: insert_v2 compare x ys
			
et rec insertion_sort_v2 compare x l =
	match l with
	| [] -> [x]
	| x :: xs -> insert_v2 compare x @@ insertion_sort_v2 compare xs
	
	
(*Definition of fold_left
  f must be binary function (takes a -> b -> a)
*)
let rec fold_left f acc l = 
	match l with
	| [] -> acc
	| x ::  xs -> fold_left f (f acc x) xs
	
(*Definition of fold_left *)
let rec fold_right f l acc =
	match l with
	| [] -> acc
	| x :: xs -> f x (fold_right f xs acc)
	
	

