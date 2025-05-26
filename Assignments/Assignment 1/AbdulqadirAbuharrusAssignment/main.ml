(*
	Abdulqadir Abuharrus
	A01351737
	Assignment 1 - main.ml containing implementation of Dijkstra's algorithm
*)
open Digraph

(*	Read a file that contains an edge on each line, where an edge is comprised of two vertices and the distance between. (2 strings and an integer).
	Any extra words are discarded.*)
let read_file src =
	let ic = open_in src in 
		let rec read_file_line acc = 
			try
				let line = input_line ic in read_file_line (line :: acc)
			with End_of_file -> close_in ic; List.rev acc
			in read_file_line []
			
(*Parse a string for first and second vertex and the distance between. Ignore everything else.*)
let parse_line line =
  let parts = String.split_on_char ' ' line in
  (* Filter out empty strings caused by multiple spaces *)
  let filtered_parts = List.filter (fun x -> x <> "") parts in
	  match filtered_parts with
	  | src :: dst :: len_str :: _ -> let len = int_of_string len_str in (src, dst, len)
	  | _ -> failwith "Invalid line format"


(*	Given a file path containing graph data, read the file and construct a directed graph
	Utilizes `read_file src` and `pare_line line` helper functions.*)
let read_graph s =
	let lines  = read_file s in
		let edges = List.map parse_line lines in Digraph.of_edges edges
		


(* Update the distance for a given vertex in an association list *)
let rec update_distance vertex new_dist dist_list =
  match dist_list with
  | [] -> [(vertex, new_dist)]
  | (v, d) :: rest -> if v = vertex then (v, new_dist) :: rest
  						else (v, d) :: update_distance vertex new_dist rest
  		
  		
(* Find the vertex with the minimum distance in the list *)
let rec find_min_vertex dist_list unvisited =
  List.fold_left (fun acc (v, d) ->
    match acc with
    | None -> if List.mem v unvisited then Some (v, d) else None
    | Some (_, d_min) ->
      if d < d_min && List.mem v unvisited then Some (v, d) else acc) None dist_list
      

(* Update the predecessor (distances) for a given vertex *)
let rec update_predecessor vertex pred pred_list =
  match pred_list with
  | [] -> [(vertex, Some pred)]
  | (v, p) :: rest ->
    if v = vertex then (v, Some pred) :: rest
    else (v, p) :: update_predecessor vertex pred rest

(* Construct the shortest path from the predecessors list *)
let rec construct_path prev_list v acc =
  match List.assoc v prev_list with
  | None -> acc
  | Some u -> construct_path prev_list u (u :: acc)

(* Implementing Dijkstra's algorithm to find the shortest path  *)
let dijkstra src dst graph =
	(*Setup vertices, list of distace, and list of previous (visited) nodes*)
  	let vertices = Digraph.vertices graph in
  	let dist_list = List.map (fun v -> (v, if v = src then 0 else max_int)) vertices in
  	let prev_list = List.map (fun v -> (v, None)) vertices in
	(* recursive helper*)
	  let rec dijkstra_helper dist_list prev_list unvisited =
		match find_min_vertex dist_list unvisited with
		| None -> failwith "No path found"
		| Some (u, d_u) ->
		  if u = dst then
		    let path = construct_path prev_list u [u] in
		    (d_u, path)
		  else
		  (* define neighboring nodes *)
		    let neighbours = neighbors u graph in
		    let (dist_list', prev_list') =
		      List.fold_left (fun (d_acc, p_acc) (v, weight) ->
		        let d_v = List.assoc v d_acc in
		        let alt = d_u + weight in
		        if alt < d_v then
		          let d_acc' = update_distance v alt d_acc in
		          let p_acc' = update_predecessor v u p_acc in
		          (d_acc', p_acc')
		        else (d_acc, p_acc)
		      ) (dist_list, prev_list) neighbours in
		    let unvisited' = List.filter (fun x -> x <> u) unvisited in
		    dijkstra_helper dist_list' prev_list' unvisited' in
	    dijkstra_helper dist_list prev_list vertices

