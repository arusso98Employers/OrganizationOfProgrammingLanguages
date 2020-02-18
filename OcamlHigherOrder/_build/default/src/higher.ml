open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let len lst = 
	fold (fun a z -> a + 1) 0 lst 

let count_greater lst target = 
	fold (fun a z -> if z > target then a + 1 else a + 0) 0 lst 


let greater_tuple lst = 
	map ( fun x -> x, (fold (fun a z -> if z > x then a + 1 else a + 0) 0 lst)) lst 


let flat_pair lst = 
	fold (fun a (xs, ys) -> a @ [xs] @ [ys]) [] lst 

let rm lst target = 
	fold (fun a z -> if z <= target then a @ [z] else a) [] lst 

