(* compile using: ocamlbuild bstree.cmo
 * to load in utop, type:
 * #directory "_build";;
 * #load "bstree.cmo";;
 * you can call a function like this: Bstree.is_empty 
 *) 
type 'a t = Leaf | Node of 'a * 'a t* 'a t

let empty = Leaf

let is_empty t = t = Leaf

let rec size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size l + size r

let rec height t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (height l) (height r)

let rec insert cmp z t =
  match t with
  | Leaf -> Node (z, Leaf, Leaf)
  | Node (x, l, r) when cmp z x < 0 ->
    Node (x, insert cmp z l, r)
  | Node (x, l, r) when cmp z x > 0 ->
    Node (x, l, insert cmp z r)
  | _ -> t

let of_list cmp l =
  List.fold_left (fun acc x -> insert cmp x acc) empty l

let rec largest t =
  match t with
  | Leaf -> failwith "largest: empty tree"
  | Node (x, _, Leaf) -> x
  | Node (_, _, r) -> largest r

let rec delete cmp z t =
  match t with
  | Leaf -> Leaf
  | Node (x, l, r) when cmp z x < 0 ->
    Node (x, delete cmp z l, r)
  | Node (x, l, r) when cmp z x > 0 ->
    Node (x, l, delete cmp z r)
  | Node (_, Leaf, r) -> r
  | Node (_, l, Leaf) -> l
  | Node (_, l, r) ->
    let max = largest l in
    Node (max, delete cmp max l, r)

let rec mem cmp z t =
  match t with
  | Leaf -> false
  | Node (x, l, _) when cmp z x < 0 ->
    mem cmp z l
  | Node (x, _, r) when cmp z x > 0 ->
    mem cmp z r
  | _ -> true
