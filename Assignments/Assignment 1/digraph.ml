(*	Abdulqadir Abuharrus
	A01351737
	Assignment 1 - digraph
*)

module VtxMp = Map.Make(String)
(* 	Declaring our edge type and Remember, edges have direction...
	The type 't' represents the directed graph, mapping a vertex to a list of (neighbor, length) *)
type t = ((string * int) list)VtxMp.t
(* Declaring our edge type and Remember, edges have direction... *)
type edge = string * string * int

exception Inv_edge
exception Inv_graph

let empty = VtxMp.empty

let rec list_contains ls x =
	match ls with
	| [] -> false
	| hd::tl -> if hd = x then true else list_contains tl x 

(*	Add an edge to the graph, 
	iff the vertices are valid strings, otherwise raise Inv_edge.
	If it's a duplicate it must have the same length, otherwise raise Inv_graph.*)
let add_edge ((v1, v2, ed_len):edge) (graph : t) :t=
  if v1 = "" || v2 = "" || ed_len <= 0 then raise Inv_edge
  else
    try
      let existing_edges = VtxMp.find v1 graph in
      
      (* Check for duplicate edges with different lengths *)
      if List.exists (fun (dest, len) -> dest = v2 && len <> ed_len) existing_edges then raise Inv_graph
      		else
				(* If edge already exists with the same length, don't change *)
				if List.exists (fun (dest, len) -> dest = v2 && len = ed_len) existing_edges then graph
				else
				  (* Add the new edge *)
				  let updated_edges = (v2, ed_len) :: existing_edges in
				  VtxMp.add v1 updated_edges graph
			with
			| Not_found ->
				(* If v1 is not in the graph, add a new entry in adjacency list *)
				VtxMp.add v1 [(v2, ed_len)] graph

(* From a list of edges, return a digraph of all those edges*)
let of_edges (ls: edge list) :t =
	let rec helper acc ls =
		match ls with
		| [] -> acc
		| hd::tl -> helper (add_edge hd acc) tl
	in helper empty ls
	
	
(* From a digraph, return a sorted list of all the distinct edges*)
let edges (graph: t) : edge list =
  let compare_edges (v1, v2, _) (va, vb, _) =
    let cmp1 = String.compare v1 va in
    if cmp1 = 0 then String.compare v2 vb else cmp1 in
  (* Extract and flatten the list of all edges from the map *)
  let all_edges =
  (*VtxMp.fold traverses the entire map, collecting `src` verts and destination vertices `dest`*)
    VtxMp.fold (fun src neighbors acc ->
      List.fold_left (fun acc (dest, length) -> (src, dest, length) :: acc) acc neighbors) graph [] in
  List.sort_uniq compare_edges all_edges

(* Returns a list of all the  distinct vertices in a alphabetical order*)
let vertices (graph: t) : string list =
  let vertex_set = 
    VtxMp.fold (fun src neighbors acc ->
      let acc = src :: acc in
      List.fold_left (fun acc (dest, _) -> dest :: acc) acc neighbors) graph [] in
  (* Remove duplicates and sort the list *)
  List.sort_uniq String.compare vertex_set

(*	For a specific vertex `vtx` find and return a list of all its neighbors*)
let neighbors vtx (graph: t) : (string *int) list =
	let verts = VtxMp.find_opt vtx graph in
		match verts with
		| Some neighbors -> neighbors
		| None -> [] (*empty if no neighbors...lonely vertices :( *)
	
