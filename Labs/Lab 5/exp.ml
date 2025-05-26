(*
	Abdulqadir Abuharrus
	A01351737
	Lab 5
*)

type 'a lazystream  = Cons of 'a *'a lazystream Lazy.t
let rec from n = Cons(n, lazy (from @@ n + 1))
let natur = from 1

let hd (Cons (h, _)) = h
let tl (Cons (_, tl)) = Lazy.force tl

let rec take n (Cons (h,t)) = 
	if n > 0 then h :: take (n - 1) (Lazy.force t) else []
	
	

(* `exp_terms flt` functions, takes a float and produces the lazy stream of exponentials
	starts 1 + flt + flt^2/2! + flt^3/3! + ... 
*)
	
(* Factorial helper function*)
let rec factorial n =
  if n <= 1 then 1 else n * factorial (n - 1)

let exp_terms flt = 
  let rec exp_helper n acc = 
    let term = (Float.pow flt (Float.of_int n)) /. (Float.of_int (factorial n)) in
    Cons(term, lazy (exp_helper (n + 1) (acc +. term)))
  in exp_helper 0 0.0

let first_twenty = List.fold_left (+.) 0.0 (take 20 (exp_terms 1.1))
