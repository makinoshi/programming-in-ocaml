(* 1 *)
let rec mem a s = match s with
    [] -> false
  | fst :: rst ->
    if a = fst then true
    else mem a rst

let _ = mem 2 [1; 2; 3]
let _ = mem 4 [1; 2; 3]

(* 2 *)
let rec forld_left f e l =
  match l with
    [] -> e
  |  x :: rest -> forld_left f (f e x) rest

let _ = forld_left (+) 0 [1; 2; 3; 4]

let intersect s1 s2 =
  forld_left (fun acc x -> if mem x s2 then x :: acc else acc) [] s1

let _ = intersect [1; 2; 3; 4] [2; 4; 6; 8]

(* 3 *)
let distinct l = forld_left (fun acc x -> if mem x acc then acc else x :: acc) [] l

let _ = distinct [1; 2; 3]
let _ = distinct [1; 2; 3; 2]

let union s1 s2 = distinct (s1 @ s2)

let _ = union [1; 2; 3; 4] [2; 4; 6; 8]

(* 4 *)
let diff s1 s2 =
  forld_left (fun acc x -> if mem x s2 then acc else x :: acc) [] s1

let _ = diff [1; 2; 3] [3; 4]
