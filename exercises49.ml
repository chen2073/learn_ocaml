(* Exercise: twice, no arguments *)
let double x = 2 * x
let square x = x * x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square (* type inference and lazy evaluation *)

(* Exercise: mystery operator 1 *)
let ( $ ) f x = f x (* function application *)

(* Exercise: mystery operator 2 *)
let ( @@ ) f g x = x |> g |> f

(* Exercise: repeat *)
let rec repeat f n x = 
  match n with 
  | 0 -> x
  | _ -> repeat f (n-1) (f x)

(* Exercise: product *)
(* f must be associative and commutative *)
let rec fold_right f acc = function
| [] -> acc
| head :: rest -> f head (fold_right f acc rest)

let rec fold_left f acc = function
| [] -> acc
| head :: rest -> fold_left f (f acc head) rest

(* let product_left ls = fold_left ( *. ) 1. ls
let product_right ls = fold_right ( *. ) 1. ls *)

(* Exercise: terse product *)
let product_left = fold_left ( *. ) 1.
let product_right = fold_right ( *. ) 1.

(* Exercise: sum_cube_odd  *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
let ( -- ) i j = from i j []

let rec map f = function
  | [] -> []
  | h :: t -> f h :: map f t

let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let sum_cube_odd n = 0 -- n 
  |> filter (fun x -> x mod 2 = 0)
  |> map (fun x -> x * x * x)
  |> fold_left (+) 0

(* Exercise: sum_cube_odd pipeline *)

(* Exercise: exists *)
let rec exists_rec pred ls = 
  match ls with 
  | [] -> false
  | head :: rest -> pred head || exists_rec pred rest

let exists_fold pred ls = map pred ls |> List.fold_left (||) false  

let exists_lib pred ls = List.exists

(* Exercise: account balance *)
let rec get_balance_rec balance debits_ls = 
  match debits_ls with 
  | [] -> balance
  | head :: rest -> get_balance_rec (balance -. head) rest

let get_balance_left balance debits_ls = fold_left (-.) balance debits_ls

let get_balance_right balance debits_ls = balance -. fold_right (+.) 0. debits_ls

(* Exercise: library uncurried *)
let list_append (ls1, ls2) = List.append ls1 ls2
let char_compare (a, b) = Char.compare a b
let max (a, b) = Stdlib.max a b

(* map composition *)
let compose f g x = f (g x)
(* List.map (compose f g) ls *)

(* Exercise: more list fun *)
let find_long_strings = filter (fun s -> String.length s > 3)
let add1f = map (fun x -> x +. 1.)
let join strs sep = 
  match strs with 
  | [] -> ""
  | first_str :: rest_strs -> List.fold_left (fun acc str -> acc ^ sep ^ str) first_str rest_strs

(* Exercise: association list keys *)
let keys (assoc_ls: ('a * 'b) list): 'a list = 
  let custom_compare (k1, v1: 'a * 'b) (k2, v2: 'a * 'b): int = 
    Stdlib.compare k1 k2 in 
  let rec get_keys acc assoc_ls = 
    match assoc_ls, acc with 
    | [], _ -> acc
    | (k, v) :: rest, [] -> get_keys ([k] @ acc) rest
    | (k, v) :: rest, latest_elem::rest_acc -> if k = latest_elem then get_keys acc rest else get_keys ([k] @ acc) rest in
  List.sort custom_compare assoc_ls |> get_keys []

let ass_ls = [(3, "three"); (1, "one"); (2, "two"); (1, "yi"); (3, "san")];;


let is_valid_matrix (matrix: int list list): bool =
  let rec helper (matrix: int list list) (cur_length: int option): bool = 
    match matrix, cur_length with 
    | [], None -> false
    | [], Some length -> true
    | head :: rest, None -> helper rest (Some (List.length head))
    | head :: rest, Some length when List.length head = length -> helper rest (Some length)
    | head :: rest, Some length when (List.length head) <> length -> false [@@ocaml.warning "-8"] in 
  helper matrix None

let is_valid_matrix_short (matrix: int list list): bool = 
  match matrix with
  | [] -> false
  | row :: rows -> let length = List.length row in 
    map (fun row -> List.length row = length) rows |> fold_left (&&) true

let m1 = [[1; 1; 1]; [9; 8; 7]]
let m2 = []
let m3 = [[1; 2]; [3]]

(* Exercise: row vector add *)
let add_row_vectors (ls1: int list) (ls2:int list): int list = List.map2 (fun x y -> x + y) ls1 ls2

let add_matrices (m1: int list list) (m2: int list list): int list list = List.map2 add_row_vectors m1 m2

let multiply_matrices (m1: int list list) (m2: int list list): int list list = 