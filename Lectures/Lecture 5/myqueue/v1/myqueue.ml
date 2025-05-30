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
