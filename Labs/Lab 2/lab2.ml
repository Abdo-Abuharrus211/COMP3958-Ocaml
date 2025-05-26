(*
	ABDULQADIR ABUHARRUS
	A01351737
	SET V
	LAB 2
*)

(* the (drop f lst) function takes a function f that returns a bool, and a list of elements lst.
While (f e) ,where e is an element from lst, returns true drop elements, else returns a list of the remaining elements *)
let rec drop_while f lst = 
	match lst with
	| [] -> []
	| x :: xs -> if f x then drop_while f xs
			else x :: xs
			
(* Test suite for drop_while function*)		
let drop_while_test () =
	assert(drop_while (fun x -> x mod 2 = 0) [4;2;6;7;6;8;1] = [7;6;8;1]);
	assert(drop_while (fun x -> x mod 2 <> 0) [4;2;6;7;6;8;1] = [4;2;6;7;6;8;1]);
	assert(drop_while (fun x  -> x <> 0) [4;2;0;7;] = [0;7]);
	assert(drop_while (fun x -> x = x) [] = [])
	
	
	
(*length of list l helper function...*)
let rec length l = 
	match l with
	| [] -> 0
	| _ :: xs -> 1 + length xs

(*Test suite for length*)
let length_test () =
	assert(length [] = 0);
	assert(length ["a"] = 1);
	assert(length [1;2;3] = 3)

	
(*	zip elements of two lists, `lst1`and `lst2` together while applying function f.
	Precondition: lst1 and lst2 must not be empty,i,e, length lst1 > 0 and length lst2 > 0.
*)	
let zip_with f lst1 lst2 =
	let rec zip_with_helper acc lst1 lst2= 
		match (lst1, lst2)  with
		|([], _) | (_,[]) -> List.rev acc
		| (x :: xs, y :: ys) -> 
		zip_with_helper ((f x y) :: acc) xs ys
		in zip_with_helper [] lst1 lst2
		
(*Test suite for zip_with*)
let zip_with_test () =
	assert(zip_with ( * ) [] [] = []);
	assert(zip_with ( - ) [3;3;3] [1;1;1] = [2;2;2]);
	assert(zip_with ( + ) [3;3;3] [] = []);
	assert(zip_with ( * ) [0;5;3] [6;9;2;10] = [0;45;6]);
	assert(zip_with ( * ) [0;5;3;22;7] [6;9;2;] = [0;45;6])


(*	Implements a custom map function that iterates over a list `lst` and pairs every element with its index 
	in the list then returns a list of tuples of the elements and their index. *)
let mapi f lst =
	let rec mapi_helper i acc lst = 
		match lst with
		| [] -> List.rev acc
		| x :: xs -> mapi_helper (i + 1) ((f i x) :: acc) xs
	  in mapi_helper 0 [] lst
	
	
(*Test suite for mapi*)
let mapi_test () =
	assert(mapi (fun i x -> (i,x)) [] = []);
	assert(mapi (fun i x -> (i,x)) [1;2;3] = [(0, 1); (1, 2); (2, 3)]);
	assert(mapi (fun i x -> (i,x)) ["a"; "b"] = [(0,"a");(1,"b")])


(*	Determine if m is divisible by n, i,e, modulo returns zero; return true if yes and false otherwise.*)
let mod_true m n = if m mod n = 0 then true else false

(*Test suite for mod_true*)
let mod_true_test () =
	assert(mod_true 1 1 = true);
	assert(mod_true 8 6 = false);
	assert(mod_true 2 2 = true)

let every n lst = 
	let filtered = mapi (fun i x -> (i, x)) lst |> List.filter (fun (i, _) -> mod_true (i+1) n)
		|> List.map snd
		in filtered

let every_test () =
	assert(every 1 [] = []);
	assert(every 1 ["Hello"] = ["Hello"]);
	assert(every 2 [1;2;56;63] = [2;63]);
	assert(every 2 ["a";"b";"c";"d";"f"] = ["b";"d"]);
	assert(every 3 ["a";"b";"c";"d";"f"] = ["c"])

(*	Remove duplicates from the list.
	To be used within List.fold_left*)

(*	Group function helper, check if e is equal to hd
	If e equal to head then add to list
	else, add to list of e to accumulator.*)
let grp_aux acc e = 
		match acc with
		| [] -> [[e]]
		| (hd :: tl) :: xs when hd = e -> (e :: hd :: tl) :: xs
		| _ -> [e] :: acc
		
(*	Group identical elements in a list lst by 'combining' them (removing the duplicates), then return a list with all duplicates removed.
	Utilizes helper function dup_elem.
	*)
let group lst = List.fold_left grp_aux [] lst |> List.rev

let group_test () =
	assert(group [1;1;2;6;2;78;22;3;2] = [[1; 1]; [2; 2; 2]; [3]; [6]; [22]; [78]]);
	assert(group [] = []);
	assert(group [1] = [[1]]);
	assert(group ["a";"b";"a"] = [["a";"a"]; ["b"]])

(*	Uses the group function to group count how many times an element occurs; returns list of lists containing
	elements and their count in the format (elm, cnt).*)	
let frequencies lst =
	let grouped = group (List.sort compare lst) in
		(* Map each group to a tuple (elm, cnt), took me a bit to wrap my head around this *)
  		List.map (fun g -> (List.hd g, List.length g)) grouped

let frequencies_test () =
	assert(frequencies [] = []);
	assert(frequencies [23;12;15;12;45;15;13;45;15;12;15;15] = [(12, 3); (13, 1); (15, 5); (23, 1); (45, 2)]);
	assert(frequencies ["a";"b";"a"] = [("a", 2); ("b", 1)])
