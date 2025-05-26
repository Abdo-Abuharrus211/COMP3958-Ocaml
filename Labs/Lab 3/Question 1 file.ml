(*	Lab 3, Question 1 file
	Abdulqadir Abuharrus
*)

(*	Accept a non-negative integer, return a list of digits from the number.
	If number <=0 -> fail with message*)
let digits number =
	if number = 0 then [0]
	else
		let rec digit_helper acc number = 
			match number with
			| _ when number <= 0 -> acc
			| n -> digit_helper ((n mod 10) :: acc) (n / 10)
			in digit_helper [] number

(*	Test suite for digits *)
let digits_test () =
	assert(digits (0) = []);
	assert(digits (100) = [1;0;0]);
	assert(digits (0100) = [1;0;0]);
	assert(digits (1) = [1])
	
(*	Using List.fold_  return a single integer from a list of integers, by combining.*)
let int_of_digits lst = if List.length lst = 0 then failwith "List is empty."
	else List.fold_left (fun acc x -> acc * 10 + x) 0 lst

(*	Test suite for int_of_digits *)
let int_of_digits_test () =
	assert(int_of_digits [0] = 0);
	assert(int_of_digits [0; 1; 0] = 10);
	assert(int_of_digits [0;3;2;7;6] = 3276)

let list_of_string str = String.fold_left (fun acc x -> x :: acc) [] str |> List.rev

(*	Test suite for list_of_string *)
let list_of_string_test () =
	assert(list_of_string "" = []);
	assert(list_of_string "BOB" = ['B';'O';'B']);
	assert(list_of_string "Yeah" = ['Y';'e';'a';'h'])

(*	Using the `list_of_string` function determine if two strings, s1 and s2, are permutations of each other.*)
let is_permutation s1 s2 =
  let lst1 = list_of_string s1 in
  let lst2 = list_of_string s2 in
  List.length lst1 = List.length lst2 &&
  (lst1 |> List.sort compare) = (lst2 |> List.sort compare)
 

(*	Test suite for is_permutation*)
let is_permutation_test () =
	assert(is_permutation "Bob" "oBb" = true);
	assert(is_permutation "BOB" "Bob" = false);
	assert(is_permutation "" "Hi" = false);
	assert(is_permutation "" "" = true);
	assert(is_permutation " " " " = true);
	assert(is_permutation "hello" "leolh" = true);
	assert(is_permutation "hello" "hell" = false)
	
	

