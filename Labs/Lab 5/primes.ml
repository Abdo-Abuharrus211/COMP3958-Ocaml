(*
	Abdulqadir Abuharrus
	A01351737
	Lab 5
*)

type 'a infstream = Cons of 'a * (unit -> 'a infstream)
let rec from n = Cons (n, fun () -> from (n + 1))
(* Also start from 1, and define hd and tl*)
let nats = from 1
let hd (Cons (h, _)) = h
let tl (Cons (_, t)) = t ()


(*	My own attempt at [Take n s]
	Return a list of `n` items from a the stream `s` *)
let rec take n (Cons(hd, tl)) = 
	if n > 0 then hd :: (take (n-1) (tl ())) else []


(*Filter function: `filter f s`, takes a substream of elements `s` that satisfy predicate `f`. Analogous to List.filter of infintie streams.
val filter: ('a -> bool) -> 'a infstream -> 'a infstream

Doesn't terminate if no element satisfies `f`.
*)
let rec filter f (Cons(hd, tf)) =
	if f hd then Cons (hd, fun () -> filter f (tf ()))
		else filter f (tf ())

let filter_test () =
	assert(take 10 (filter (fun x -> x mod 2 = 0) nats) = [2; 4; 6; 8; 10; 12; 14; 16; 18; 20]);
	assert(take 10 (filter (fun x -> (x + 2) mod 3 = 0) nats) = [1; 4; 7; 10; 13; 16; 19; 22; 25; 28])


(*	`primes` is an infinite stream based on the 'Sieve of Eratosthenes' that only returns the infinite stream of prime numbers using the `filter f s` function.
	Done by `take 100` on the primes stream
*)


(* is_prime Helper function*)
let is_prime num =
  if num < 2 then false
  else
    let rec is_not_divisible_from i =
      i * i > num || (num mod i <> 0 && is_not_divisible_from (i + 1))
    in
    is_not_divisible_from 2

(* The infinite stream of prime numbers *)
let primes_stream = 
  let rec sieve s =
    let h = hd s in (* The `hd` funciton gets the head of the stream `s` *)
    Cons(h, fun () -> sieve (filter (fun x -> x mod h <> 0) (tl s)))
  in
  sieve (from 2)

let primes = take 100 primes_stream  
