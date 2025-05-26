module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt 
  type t

  val empty : t
  val is_empty : t -> bool
  val insert : elt -> t -> t
  val of_list : elt list -> t
  val delete : elt -> t -> t
  val mem : elt -> t -> bool
end

module Make (Ord : OrderedType) = struct
  type elt = Ord.t
  type t = Leaf | Node of elt * t * t

  let empty = Leaf

  let is_empty t = t = Leaf

  let rec insert x t =
    match t with
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (x', l, r) when Ord.compare x x' < 0 ->
      Node (x', insert x l, r)
    | Node (x', l, r) when Ord.compare x x' > 0 ->
      Node (x', l, insert x r)
    | _ -> t

  let of_list l =
    List.fold_left (Fun.flip insert) empty l

  let rec largest t =
    match t with
    | Leaf -> failwith "largest: empty tree"
    | Node (x, _, Leaf) -> x
    | Node (_, _, r) -> largest r

  let rec delete z t =
    match t with
    | Leaf -> Leaf
    | Node (x, l, r) when Ord.compare z x < 0 ->
      Node (x, delete z l, r)
    | Node (x, l, r) when Ord.compare z x > 0 ->
      Node (x, l, delete z r)
    | Node (_, Leaf, r) -> r
    | Node (_, l, Leaf) -> l
    | Node (_, l, r) ->
      let max = largest l in
      Node (max, delete max l, r)

  let rec mem z t =
    match t with
    | Leaf -> false
    | Node (x, l, _) when Ord.compare z x < 0 ->
      mem z l
    | Node (x, _, r) when Ord.compare z x > 0 ->
      mem z r
    | _ -> true
end
