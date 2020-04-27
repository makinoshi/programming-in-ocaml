let pi = 3.14

let area_of_circle r =
  r *. r *. pi

let area_of_circle(r : float) : float =
  r *. r *. pi

let _ = area_of_circle 2.0

let x = 1 and y = 2;;

(* Q 3.1 *)
(* 1 *)
let one_daller = 114.32
let daller_yen d =
  int_of_float(floor(one_daller *. d +. 0.5))
let ret = daller_yen 12. ;;

(1.0, 2.0)

let author = ("Atsushi", "Igarashi", 174.0, 61.0)
let big_tuple = ((3, 'a'), (9.3, "Hello", false))

let (firstname, lastname, height, weight) = author
let (x, y) = big_tuple
let (f, s, b) = y
let (x, (f, s, b)) = big_tuple

(* let average p =
 *   let (x, y) = p in
 *   (x +. y) /. 2.0 *)

let average (x, y) = (x +. y) /. 2.0
let _ = average (2.5, 4.8);;

(* Q 3.4.4 *)
sqrt 4.0;;
let geo_mean (x, y) =
  sqrt(x *. y)

let _ = geo_mean (2.0, 2.0)

let bmi_msg (name, h, w) =
  let h = h /. 100. in
  let bmi = w /. (h *. h) in
  if bmi < 18.5 then name ^ "さんは痩せています"
  else if 18.5 <= bmi && bmi < 25. then name ^"さんは標準です"
  else if 25. <= bmi && bmi < 30. then name ^ "さんは肥満です"
  else name ^ "さんは高度肥満です"

let a = bmi_msg ("foo", 170., 50.)
let b = bmi_msg ("foo", 170., 70.)
let c = bmi_msg ("foo", 170., 90.)

let rec fact n =
  if n = 1 then 1
  else fact(n-1) * n

let _ = fact 1
let _ = fact 3
let _ = fact 10

(* let rec fib n =
 *   if n = 1 || n = 2 then 1
 *   else fib(n - 1) + fib(n - 2) *)

(* let rec fib_pair n =
 *   if n = 1 then (1, 0)
 *   else
 *     let (i, j) = fib_pair(n - 1) in
 *     (i + 1, j) *)

let rec fib n =
  let rec fib_pair n =
    if n = 1 then (1, 0)
    else
      let (i, j) = fib_pair(n - 1) in
      (i + j, i)
  in
  let (i, _) = fib_pair n in
  i

let _ = fib 1
let _ = fib 10
let _ = fib 30
let _ = fib 100

(* Euclidean Algorithm *)
let rec euclidean (m, n) =
  let (m, n) = if (m >= n) then (m, n) else (n, m) in
  if n = 0 then m
  else euclidean (n, m mod n)

let _ = euclidean(3, 1)
let _ = euclidean(8, 4)
let _ = euclidean(1071, 1029)

(* nCm *)
let rec combination (n, m) =
  if m = 0 || (n = m) then 1
  else combination (n - 1, m) + combination (n - 1, m - 1)

let _ = combination (3, 0)
let _ = combination (3, 3)
let _ = combination (3, 1)
let _ = combination (5, 3)
let _ = combination (6, 3)

let rec even n =
  if n = 0 then true else odd (n - 1)
and odd n =
  if n = 0 then false else even (n - 1)

let _ = even 6
let _ = odd 14

let rec pos n =
  neg (n - 1) +. 1.0 /. (float_of_int (4 * n + 1))
and neg n =
  if n < 0 then 0.0
  else pos (n) -. 1.0 /. (float_of_int (4 * n + 3))

let _ = 4.0 *. pos 200
let _ = 4.0 *. pos 800

let rec interfact (i, res, n) = (* iterative factorial *)
  if i = n then res * i
  else interfact (i+1, res * i , n)

let fact n = interfact (1, 1, n)

let _ = fact 4

let rec tailfact (n, res) =
  if n = 1 then res
  else tailfact (n - 1, n * res)

let _ = tailfact (4, 1)

(* Q 3.5.5 *)
let rec pow1 (x, n) =
  if n = 0 then 1.0
  else x *. pow1 (x, n - 1)

let _ = pow1 (1.0, 0)
let _ = pow1 (2.0, 5)
let _ = pow1 (3.0, 2)

let rec pow2 (x, n) =
  if n = 0 then 1.0
  else
    let half = pow1 (x, n/2) in
    if (n mod 2) = 0 then half *. half
    else half *. half *. x

let _ = pow2 (1.0, 0)
let _ = pow2 (2.0, 5)
let _ = pow2 (3.0, 2)
let _ = pow2 (3.0, 10)

let rec iterfib n =
  if n = 0 then 0
  else if n = 1 then 1
  else
    (* ret1 has Fib(n-1) *)
    (* ret2 has Fib(n-2) *)
    let rec fib (i, ret1, ret2) =
      if i = n then ret1 + ret2
      else fib (i + 1, ret1 + ret2, ret1)
    in
    fib (2, 1, 0)

let _ = iterfib 0
let _ = iterfib 1
let _ = iterfib 3
let _ = iterfib 5
let _ = iterfib 10

let rec sum_of (f, n) =
  if n = 0 then 0 else sum_of (f, n-1) + f n

let sum_of_cube n = sum_of ((fun x -> x * x * x), n)

let _ = sum_of_cube 5

let concat_curry s1 = fun s2 -> s1 ^ s2 ^ s1

let _ = (concat_curry "foo") "bar"

let concat_curry = fun s1 s2 -> s1 ^ s2 ^ s1

let _  = concat_curry "foo"
let _ = (concat_curry "_") "foo"

let concat_curry s1 s2 = s1 ^ s2 ^ s1

let x  = concat_curry "foo"
let _ = x "bar"
let _ = (concat_curry "_") "foo"

let _ = ~- 3 + ~- 5

(* let _ = abs -3 *)
let _ = abs ~- 3

(* 3.6 *)

let deriv f =
  let dx = 0.1e-10 in
  fun x -> (f(x +. dx) -. f(x)) /. dx

let _ = deriv (fun x -> x *. x *. x) 3.0

let fixpoint f init =
  let threshold = 0.1e-10 in
  let rec loop x =
    let next = f x in
    if abs_float (x -. next) < threshold then x
    else loop next in
  loop init

let newton_transform f = fun x -> x -. f(x) /. (deriv f x)

let newton_method f guess = fixpoint (newton_transform f) guess

let square_root a = newton_method (fun x -> x *. x -. a) 1.0

let _ = square_root 5.0
