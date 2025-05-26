module type S = sig
  type 'a t
  exception Empty

  val empty : 'a t

  val is_empty : 'a t -> bool

  val enqueue : 'a -> 'a t -> 'a t

  val dequeue : 'a t -> 'a t

  val dequeue_opt : 'a t -> 'a t option

  val front : 'a t -> 'a

  val front_opt : 'a t -> 'a option

  val to_list : 'a t -> 'a list
end

module ListQueue = struct
  type 'a t = 'a list
  exception Empty

  let empty = []

  let is_empty q = q = []

  let enqueue x q = q @ [x]

  let dequeue = function
    | [] -> raise Empty
    | _ :: xs -> xs

  let dequeue_opt = function
    | [] -> None
    | _ :: xs -> Some xs

  let front = function
    | [] -> raise Empty
    | x :: _ -> x

  let front_opt = function
    | [] -> None
    | x :: _ -> Some x

  let to_list q = q
end

module TwoListQueue = struct
  type 'a t = 'a list * 'a list

  exception Empty

  let empty = ([], [])

  let is_empty (l, _) = l = []  (* queue is empty iff first list is empty *)

  let enqueue x q =
    match q with
    | [], l -> [x], l
    | l, l' -> l, x :: l'

  let dequeue q =
    match q with
    | [], _ -> raise Empty
    | [_], l -> List.rev l, []
    | _ :: xs, l -> xs, l

  let dequeue_opt q =
    match q with
    | [], _ -> None
    | [_], l -> Some (List.rev l, [])
    | _ :: xs, l -> Some (xs, l)

  let front = function
    | [], _ -> raise Empty
    | x :: _, _ -> x

  let front_opt = function
    | [], _ -> None
    | x :: _, l -> Some x

  let to_list (l, l') = l @ List.rev l'
end
