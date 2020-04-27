let fst_int ((x, y) : int * int) = x

let fst (x, y) = x

let apply f x = f x

let twice f x = f (f x)

let _ = twice (fun x -> x + 1) 3
let _ = twice (fun s -> "<" ^ s ^ ">") "abc"
let _ = twice twice (fun x -> x + 1) 3
let _ = twice twice (fun s -> "<" ^ s ^ ">") "abc"

let ($) f g x = f (g x)

let _ = (( ~-. ) $ sqrt) 2.0

let id x = x

let k x y = x

(* let const17 = k 17 in const17 4.0 *)

let k x y = x

let s x y z = x z (y z)

let _ = s k k 5

(* Q 4.4 *)
let curry f x y = f (x, y)

let average (x, y) = (x +. y) /. 2.0

let curried_avg = curry average

let _ = average (4.0, 5.3)
(* let _ = curried_avg 4.0 *)
let _ = curried_avg 4.0 5.3

let uncurry f (x, y) = f x y

let _ = let avg = uncurry curried_avg in
  avg (4.0, 5.3)

let rec repeat f n x =
  if n > 0 then repeat f (n - 1) (f x)
  else x

let fib n =
  let (fibn, _) = repeat (fun (a, b) -> (b, a + b)) n (0, 1) in
  fibn

let _ = fib 4
let _ = fib 10

let rec funny f n =
  if n = 0 then id
  else if n mod 2 = 0 then funny (f $ f) (n / 2)
  else funny (f $ f) (n / 2) $ f
