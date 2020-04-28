let coll = 64 :: 128 :: [256]

let rec sum_list l =
  match l with
    [] -> 0
  | n :: rest -> n + (sum_list rest)

let rec length = function
    [] -> 0
  | _ :: rest -> 1 + length rest

let _ = length [1; 2; 3]

let rec append l1 l2 =
  match l1 with
    [] -> l2
  | first :: rest -> first :: (append rest l2)

let _ = append [1; 2] [4; 5]
let _ = append [] [4; 5]

let rec reverse l =
  match l with
    [] -> l
  | first :: rest -> reverse rest @ [first]

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | first :: rest -> rev_append rest (first :: l2)

let _ = rev_append [1; 2] [3 ;4]
let _ = rev_append [1; 2] []

let rec map f = function
    [] -> []
  | x :: rest -> f x :: map f rest

let rec forall p = function
    [] -> true
  | x :: rest -> if p x then forall p rest else false

let _ = forall (fun x -> x > 5) [9; 20; 6]

let rec exists p = function
    [] -> false
  | x :: rest -> (p x) || (exists p rest)

let _ = exists (fun x -> x > 5) [1; 2; 3]

let rec forld_right f l e =
  match l with
    [] -> e
  | x :: rest -> f x (forld_right f rest e)

let rec forld_left f l e =
  match l with
    [] -> e
  |  x :: rest -> forld_left f (f l e) rest

(* let rec nth n l =
 *   if n = 1 then
 *     let a :: _ = l in a
 *   else
 *     let _ :: rest = l in nth (n - 1) rest *)

(* let rec nth n l =
 *   match n with
 *     1 -> let a :: _ = l in a
 *   | _ -> let _ :: rest = l in nth (n - 1) rest *)

(* let rec nth n l =
 *   match (n , l) with
 *     (1, a :: _) -> a
 *   | (_, _ :: rest) -> nth (n - 1) rest *)

(* let rec nth n l =
 *   match (n , l) with
 *     (1, a :: _) -> a
 *   | (n', _ :: rest) when n' > 0 -> nth (n - 1) rest
 *
 * let rec assoc a = function
 *   (a' , b) :: rest -> if a = a' then b else assoc a rest
 *
 * let _ = assoc "b" [("a", "foo"); ("b", "bar")]
 *
 * let rec invalid_assoc a = function
 *     (a, b) :: rest -> b
 *   | _ :: rest -> invalid_assoc a rest
 *
 * let _ = invalid_assoc "b" [("a", "foo"); ("b", "bar")] *)

let nextrand seed =
  let a = 16807.0 and m = 2147483647.0 in
  let t = a *. seed
  in t -. m *. floor (t /. m)
  let rec randlist n seed tail =
    if n = 0 then (seed, tail)
    else randlist (n - 1) (nextrand seed) (seed::tail)

(* insertion sort *)
let rec insert x = function
    [] -> [x]
  | y :: rest when x < y -> x :: y :: rest
  | y :: rest -> y :: (insert x rest)

let _ = insert 3 [1; 2; 5]

let rec insertion_sort = function
    [] -> []
  | x :: rest -> insert x (insertion_sort rest)

let _ = insertion_sort (snd (randlist 10 1.0 []))

(* quick sort *)
let rec partition left right pivot = function
    [] -> (left, right)
  | y :: rest ->
    if pivot < y then partition left (y::right) pivot rest
    else partition (y::left) right pivot rest

let _ = partition [] [] 7 [9; 1; 5; 4; 18]

let rec quick_sort = function
    ([] | [_]) as l -> l
  | pivot :: rest ->
    let rec partition left right = function
        [] -> (quick_sort left) @ (pivot :: quick_sort right)
      | y :: ys ->
        if pivot < y then partition left (y::right) ys
        else partition (y::left) right ys
    in
    partition [] [] rest

let _ = quick_sort (snd (randlist 10 1.0 []))
let _ = quick_sort (snd (randlist 10 1.0 [])) = insertion_sort (snd (randlist 10 1.0 []))

(* let _ = quick_sort (snd (randlist 10000 1.0 []))
 * let _ = insertion_sort (snd (randlist 10000 1.0 [])) *)
