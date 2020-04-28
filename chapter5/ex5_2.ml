(* 1 *)
let rec downto1 n =
  if n = 0 then []
  else n :: downto1 (n - 1)

let _ = downto1 6

(* 2 *)
let rec assoc key = function
  (key', value) :: rest -> if key = key' then value else assoc key rest

let rec repeat_str s n =
  if n = 0 then ""
  else s ^ (repeat_str s (n - 1))

let dict1 = [(1000,"M"); (500,"D"); (100,"C"); (50,"L"); (10,"X"); (5,"V"); (1,"I")]
let dict2 = [(1000,"M"); (900,"CM"); (500,"D"); (400,"CD"); (100,"C"); (90,"XC"); (50,"L"); (40,"XL"); (10,"X"); (9,"IX"); (5,"V"); (4,"IV"); (1,"I")]

let rec roman dict n = match dict with
    [] -> ""
  | (base, s) :: rest_dict ->
    let quotient = n / base in
    repeat_str s quotient ^ roman rest_dict (n - (base * quotient))

let _ = assoc 50 dict1
let _ = repeat_str "M" 3
let _ = repeat_str "D" 1
let _ = roman dict1 1984 = "MDCCCCLXXXIIII"
let _ = roman dict2 1984 = "MCMLXXXIV"

(* 3 *)
let rec length = function
    [] -> 0
  | _ :: rest -> 1 + length rest

let rec nested_length lst = match lst with
    [] -> 0
  | first :: rest ->
    length first + nested_length rest

let _ = nested_length[[1;2;3]; [4;5]; [6]; [7;8;9;10]] = 10

(* 4 *)
let rec concat lst = match lst with
    [] -> []
  | first :: rest -> first @ concat rest

let _ = concat[[0;3;4]; [2]; []; [5;0]]

(* 5 *)
let rec zip la lb = match (la, lb) with
  (([], _) | (_ , [])) -> []
  | (a :: la, b :: lb) -> (a, b) :: zip la lb

let _ = zip[2;3;4;5;6;7;8;9;10;11] [true;true;false;true;false;true;false;false;false;true]

(* 6 *)
let rec reverse l =
  match l with
    [] -> l
  | first :: rest -> reverse rest @ [first]

let unzip l =
  let rec _unzip la lb lst =
    match lst with
      [] -> (reverse la, reverse lb)
    | (a, b) :: rest -> _unzip (a :: la) (b :: lb) rest
  in
  _unzip [] [] l

let _ = unzip (zip[2;3;4;5;6;7;8;9;10;11] [true;true;false;true;false;true;false;false;false;true])

(* 7 *)
let rec filter p lst = match lst with
    [] -> []
  | first :: rest ->
    if p first then first :: filter p rest
    else filter p rest

let _ = filter (fun i -> i > 0) [-9; 0; 2; 5; -3]

(* 8 *)
let take n lst =
  let rec _take n acc lst = match lst with
    [] -> []
    | first :: rest ->
      if n = 0 then (reverse acc)
      else _take (n - 1) (first :: acc) rest
  in
  _take n [] lst

let rec drop n lst = match lst with
    [] -> []
  | first :: rest ->
    if n = 0 then first :: rest
    else drop (n - 1) rest

let ten_to_zero = downto1 10
let _ = take 8 ten_to_zero
let _ = drop 7 ten_to_zero

(* 9 *)
let max_list lst =
  let rec _max_list x lst = match lst with
      [] -> x
    | y :: rest ->
      _max_list (max x y) rest
  in
  match lst with
    x :: rest -> _max_list x rest

let _ = max_list [7; 9; 0; -5]
