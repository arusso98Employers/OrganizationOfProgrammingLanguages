open Funs

(***********************)
(* Part 2: Integer BST *)
(***********************)

type int_tree =
  | IntLeaf
  | IntNode of int_tree * int_tree * int

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(IntLeaf, IntLeaf, x)
  | IntNode (l, r, y) when x > y -> IntNode (l, int_insert x r, y)
  | IntNode (l, r, y) when x = y -> t
  | IntNode (l, r, y) -> IntNode (int_insert x l, r, y)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (l, r, y) when x > y -> int_mem x r
  | IntNode (l, r, y) when x = y -> true
  | IntNode (l, r, y) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = 
  match t with
  | IntLeaf -> 0
  | IntNode (l, r, y) -> 1 + int_size l + int_size r


let rec int_max t = 
  match t with
  | IntLeaf -> raise (Invalid_argument "int_max")
  | IntNode (l, r, y) when r != empty_int_tree -> int_max r 
  | IntNode (l, r, y) -> y


let rec int_insert_all lst t = 
  fold (fun a z -> int_insert z a) t lst

let rec int_as_list t = 
  let rec aux t a =
    match t with
    | IntLeaf -> a
    | IntNode (l, r, y) -> 
      let a_right = aux r a in
      let a_y = y :: a_right in
      aux l a_y in
  aux t []     

(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec pinsert_aux x t f = 
  match t with
 | Leaf -> Node(x, Leaf, Leaf) 
 | Node (y, l, r) when f x y > 0 -> Node (y, l, pinsert_aux x r f)
 | Node (y, l, r) when f x y == 0 -> t
 | Node (y, l, r) -> Node (y, pinsert_aux x l f, r)

let pinsert x t = 
  match t with
  (f, ptree) -> f, pinsert_aux x ptree f

let rec pmem_aux x t f =
  match t with
  | Leaf -> false
  | Node (y, l, r) when f x y > 0 -> pmem_aux x r f
  | Node (y, l, r) when f x y == 0 -> true
  | Node (y, l, r) -> pmem_aux x l f

let pmem x t = 
  match t with
  (f, ptree) -> pmem_aux x ptree f

let pinsert_all lst t = 
  fold (fun a z -> pinsert z a) t lst

let rec p_as_list_aux t = 
  let rec aux t a =
    match t with
    | Leaf -> a
    | Node (y, l, r) -> 
      let a_right = aux r a in
      let a_y = y :: a_right in
      aux l a_y in
  aux t []     

let p_as_list t = 
  match t with 
  (f, ptree) -> p_as_list_aux ptree

let rec pmap f t = 
  let lst = map f (p_as_list t) in
  match t with 
  | (f, ptree) -> fold (fun a z -> pinsert z a) (f,Leaf) lst

(*******************************)
(* Part 4: Graphs with Records *)
(*******************************)

type node = int
type edge = { src : node; dst : node; }
type graph = { nodes : int_tree; edges : edge list; }

let empty_graph = {nodes = empty_int_tree; edges = [] }

let add_edge e { nodes = ns; edges = es } =
    let { src = s; dst = d } = e in
    let ns' = int_insert s ns in
    let ns'' = int_insert d ns' in
    let es' = e::es in
    { nodes = ns''; edges = es' }

let add_edges es g = fold (fun g e -> add_edge e g) g es

(* Implement the functions below. *)

let graph_empty g = 
  if g == empty_graph then true else false

let graph_size g = 
  if graph_empty g == true then 0 else int_size g.nodes

let is_dst n e = 
  if n == e.dst then true else false

let rec src_edges_aux n e = 
  fold (fun a z -> if z.src == n then z :: a else a) [] e

let src_edges n g = 
  src_edges_aux n g.edges

let reachable n g = failwith "unimplemented"
