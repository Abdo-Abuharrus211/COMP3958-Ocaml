(*	Lab 4
	Abdulqadir Abuharrus
*)

(*	Our record type, IDs start with 'A' and then 8 digits, the score is in range [0-100] *)
type record = {id: string; score: int}

(*	Determine if an ID is valid.
	Check if the string has exactly 9 characters and starts with 'A' *)
let is_valid_id id =
  try
    if String.length id = 9 && id.[0] = 'A' then
      let rec is_valid_helper i =
        if i = 9 then true
        else
          match id.[i] with
          | '0'..'9' -> is_valid_helper (i + 1)
          | _ -> false
      in
      is_valid_helper 1
    else
      false
  with
  | Invalid_argument _ -> false
  
(*Test suite for is_valid_id*)
let is_valid_id_test () =
	assert(is_valid_id "" = false);
	assert(is_valid_id "A" = false);
	assert(is_valid_id "Adknvo88" = false);
	assert(is_valid_id "123456789" = false);
	assert(is_valid_id "A00000001" = true);
	assert(is_valid_id "A12345678" = true)
	

(*	Determine if a score is valid; within range of [0...100] *)
let is_valid_score score = 0 <= score && score <= 100

(*Test suite for is_valid_score*)
let is_valid_score_test () =
	assert(is_valid_score 0 = true);
	assert(is_valid_score ( - 1) = false);
	assert(is_valid_score 101 = false);
	assert(is_valid_score 69 = true);
	assert(is_valid_score 100 = true)

(* Parse a string for a valid ID and score using pattern matching, then create a record.
	If ID or Score not valid return None, otherwise return Some record.
*)
let parse str =
  (* Split the string by any whitespace and filter out empty parts *)
  match String.split_on_char ' ' str |> List.filter (fun x -> x<> "") with
  | id :: score :: _ ->
    if is_valid_id id then
      try
        let score = int_of_string score in
        if is_valid_score score then Some { id; score } else None
      with Failure _ -> None
    else None
  | _ -> None  (* If there aren't at least two parts, it's invalid *)

(* Test suite for parse*)
let parse_test () =
	assert(parse "Ab " = None);
	assert(parse "A00001111   89" = Some {id = "A00001111"; score = 89});
	assert(parse "A00001111   0 Real smart one here" = Some {id = "A00001111"; score = 0});
	assert(parse "A00009999 555" = None)


let sort_records record_list = List.sort (fun x y -> if x.score <> y.score then compare y.score x.score else compare x.id y.id) record_list

(* Test suite for sort_records*)
let sort_records_test () =
  assert(sort_records [
    {id = "A00001112"; score = 89};
    {id = "A00001111"; score = 100};
    {id = "A00001113"; score = 50};
    {id = "A00001114"; score = 100};
    {id = "A00001115"; score = 50}] =
    [{id = "A00001111"; score = 100};
    {id = "A00001114"; score = 100};
    {id = "A00001112"; score = 89};
    {id = "A00001113"; score = 50}; 
    {id = "A00001115"; score = 50}])

let read_file src =
	let ic = open_in src in 
		let rec read_file_line acc = 
			try
				let line = input_line ic in read_file_line (line :: acc)
			with End_of_file -> close_in ic; List.rev acc
			in read_file_line []
			
			


(*	Read input from the user from Stdin*)
let rec read_user_input acc = 
	try
		(*let line = Scanf.scanf " %s %d" (fun x -> x) in*)
		let line = input_line stdin in
			read_user_input (line :: acc)
	with End_of_file -> List.rev acc
		
		
		
(*	Add logic for taking input and reading lines depending on arg or fileIO*)
let () =
	let input_lines = 
	if Array.length Sys.argv > 1 then
		try read_file Sys.argv.(1)
		with Sys_error msg -> Printf.printf "Error reading the file: %s\n" msg; exit 1
	else read_user_input [] in
	let records = List.filter_map parse input_lines in
  	let sorted_records = sort_records records in
  	List.iter (fun r -> Printf.printf "%d %s\n" r.score r.id) sorted_records
			
			
