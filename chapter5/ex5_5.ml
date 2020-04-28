let rec fold_right f l e = match l with
    [] -> e
  | x :: rest -> f x (fold_right f rest e)

let concat lst = fold_right (fun x acc -> x @ acc) lst []

let _ = concat[[0;3;4]; [2]; []; [5;0]]

let forall p lst = fold_right (fun x acc -> if acc then p x else acc) lst true

let _ = forall (fun x -> x > 5) [9; 20; 6]
let _ = forall (fun x -> x > 5) [9; 20; 1]

let exists p lst = fold_right (fun x acc -> if acc then acc else p x) lst false

let _ = exists (fun x -> x > 5) [1; 2; 3]
let _ = exists (fun x -> x > 5) [1; 2; 3; 7]
