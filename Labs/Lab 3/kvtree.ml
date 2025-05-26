(*	Lab 3 kvtree.ml module
	Abdulqadir Abuharrus
*)

(* Define the key-value tree type *)
type ('k, 'v) t = 
  | Leaf 
  | Node of ('k * 'v) * ('k, 'v) t * ('k, 'v) t  (* key-value pair with left and right sub-trees *)

(* Define an empty tree *)
let empty = Leaf

(* Check if the BST is empty, i,e is a Leaf.*)
let is_empty t = t = Leaf

(*	Test suite for is_empty function*)
let is_empty_test () = 
	assert(is_empty Leaf = true);
	assert(is_empty (Node ((1, "a"), Leaf, Leaf)) = false)

	
(* Recursively calculate the size of the tree *)
let rec size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size l + size r
  
(*	Test suite for size function*)
let size_test () =
	assert(size Leaf = 0);
	assert(size (Node ((1, "a"), Leaf, Leaf)) = 1);
	assert(size (Node ((1, "a"), Node ((2, "b"), Leaf, Leaf), Node ((3, "b"), Leaf, Node ((5, "x"), Leaf, Leaf)))) = 4)


(* Insert a key-value pair into the tree *)
let rec insert k v t =
  match t with
  | Leaf -> Node ((k, v), Leaf, Leaf)
  | Node ((k', v'), l, r) when k < k' -> Node ((k', v'), insert k v l, r)
  | Node ((k', v'), l, r) when k > k' -> Node ((k', v'), l, insert k v r)
  | Node ((k', _), l, r) -> Node ((k', v), l, r)
  
  
(*	Test suite for insert *)
let insert_test () =
	assert(insert 1 "a" Leaf = Node((1,"a"), Leaf, Leaf));
	assert(insert 2 "b" (Node((1,"a"), Leaf, Leaf)) = Node((1,"a"), Leaf, Node((2,"b"), Leaf, Leaf)));
	assert(insert 2 "b" (Node((1,"a"), Leaf, Leaf)) = Node((1,"a"), Leaf, Node((2,"b"), Leaf, Leaf)));
	assert(insert 0 "x" (Node((1,"a"), Leaf, Node((2,"b"), Leaf, Leaf))) = Node ((1, "a"), Node ((0, "x"), Leaf, Leaf), Node ((2, "b"), Leaf, Leaf)))
	
let rec find k t = 
  match t with
  | Leaf -> None
  | Node ((k', v'), l, r) when k > k' -> find k r
  | Node ((k', v'), l, r) when k < k' -> find k l
  | Node ((k', v'), l, r) when k = k' -> Some v'
  | _ -> None
  
(*	Test suite for find *)
let find_test () = 
	assert(find 0 Leaf= None);
	assert(find 0 (Node ((1, "a"), Node ((0, "x"), Leaf, Leaf), Node ((2, "b"), Leaf, Leaf))) = Some "x");
	assert(find 2 (Node ((1, "a"), Node ((0, "x"), Leaf, Leaf), Node ((2, "b"), Leaf, Leaf))) = Some "b");
	assert(find 99 (Node ((1, "a"), Node ((0, "x"), Leaf, Leaf), Node ((2, "b"), Leaf, Leaf))) = None)

(* Helper function to find the largest key in a sub-tree *)
let rec largest t =
  match t with
  | Leaf -> failwith "largest: empty tree"
  | Node ((k', v'), _, Leaf) -> (k', v')
  | Node (_, _, r) -> largest r

(* Delete a key from the tree *)
let rec delete k t =
  match t with
  | Leaf -> Leaf
  | Node ((k', v'), l, r) when k < k' -> Node ((k', v'), delete k l, r)
  | Node ((k', v'), l, r) when k > k' -> Node ((k', v'), l, delete k r)
  | Node ((_, _), Leaf, r) -> r
  | Node ((_, _), l, Leaf) -> l
  | Node ((_, _), l, r) ->
    let (max_k, max_v) = largest l in
    Node ((max_k, max_v), delete max_k l, r)
    
(* Test suite for of_list function *)
let delete_test () = 
	assert(delete 0 Leaf = Leaf);
	assert(delete 0 (Node ((1, "a"), Node ((0, "x"), Leaf, Leaf), Node ((2, "b"), Leaf, Leaf))) = (Node ((1, "a"), Leaf, Node ((2, "b"), Leaf, Leaf))));
	assert(delete 99 (Node ((1, "a"), Leaf, Leaf)) = Node ((1, "a"), Leaf, Leaf))
	
	
(* Convert a list of (key, value) pairs into a tree *)
let of_list l = List.fold_left (fun t (k, v) -> insert k v t) empty l

(* Test suite for of_list function *)
let of_list_test () =
  let lst = [(3, "c"); (1, "a"); (2, "b"); (4, "d")] in
  let t = of_list lst in
  assert(find 1 t = Some "a");
  assert(find 2 t = Some "b");
  assert(find 3 t = Some "c");
  assert(find 4 t = Some "d");
  assert(find 5 t = None)
	
	
