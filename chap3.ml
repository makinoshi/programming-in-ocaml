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
