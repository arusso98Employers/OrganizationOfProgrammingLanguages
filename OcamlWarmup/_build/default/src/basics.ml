(******************************)
(* Part 1: Simple Functions   *)
(******************************)

let dup a b c = 
	if a == b || b == c || a == c then true
	else false
;;

let get_list_length e = List.length e ;;

let head_divisor lst = 
	if get_list_length lst < 2 then false
	else if (List.nth lst 1) mod (List.nth lst 0)  == 0 then true
	else false
;;

let second_element lst = 
	if get_list_length lst < 2 then -1
	else List.nth lst 1
;;


let max_first_three lst = 
	if get_list_length lst == 0 then -1
	else List.fold_left (fun a x -> if x >= a then x else a) 0 lst
;;


(*********************************)
(* Part 2: Recursive Functions   *)
(*********************************)

let rec cubes_help n a = 
	let k = n - 1 in
	let m = ((n * n * n) + a) in
	if n == 0 then a else (cubes_help k m)

let rec cubes n =

	cubes_help n 0


let rec sum_odd lst = 

	List.fold_left (fun a x -> if (x mod 2 != 0) then x + a else a) 0 lst

let is_even_sum lst = 

	let sum = List.fold_left (fun a x -> a + x) 0 lst in
	if (sum mod 2 != 0) then false else true

let rec count_occ lst target = 
	List.fold_left (fun a x -> if (x == target) then a + 1 else a) 0 lst


let rec dup_list lst = 
	
	match lst with 

	|[] -> false
	|x::xt -> if count_occ xt x > 0 then true else dup_list xt


(****************)
(* Part 3: Sets *)
(****************)

let rec elem x a = 
	match a with
	|[] -> false
	|(h::t) -> if h = x then true else elem x t


let rec insert x a = 

	if (elem x a != true) then x::a else a


let rec subset a b = 
	match a with
	|[] -> true
	|h::t -> if elem h b != true then false else subset t b


let rec eq a b = 
	subset a b = true && subset b a == true 

let rec remove x a = 
	match a with 
	|[] -> a
	|h::t -> if h == x then t else h :: remove x t

let rec union a b = 
	match a with
	|[] -> b
	|h::t -> if elem h b == false then union t (h :: b) else union t b


let rec diff a b = match a with
	|[] -> []
	|(h::t) -> if elem h b = false then h :: (diff t b) else diff t b

let rec cat x a = 
	match a with 
	|[] -> []
	|h::t -> (x,h) :: (cat x t)
