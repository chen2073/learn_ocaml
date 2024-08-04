(* ex: list expressions  *)
let ls = [1;2;3;4;5]

let ls1 = 1 :: 2 :: 3 :: 4 :: 5 :: []

let ls2 = [1] @ [2;3;4] @ [5]

(* ex: product *)
let rec product_list ls = 
  match ls with 
  | [] -> 1
  | head :: rest -> head * product_list rest

(* ex: concat *)
let rec concat_list ls = 
  match ls with
  | [] -> ""
  | head :: rest -> head ^ concat_list rest

(* Exercise: product test *)

(* Exercise: patterns *)
let check_first ls = 
  match ls with 
  | head :: rest -> true
  | _ -> false

let check_length ls = 
  let rec get_length ls = 
    match ls with 
    | [] -> 0
    | _ :: rest -> 1 + get_length rest in
  match get_length ls with
  | 2 | 4 -> true
  | _ -> false

let check_equal ls = 
  match ls with
  | first :: second :: rest -> first = second
  | _ -> false

(* Exercise: library  *)
let get_5th (ls: int list): int = 
  match List.length ls with
  | length when length < 5 -> 0
  | _ -> List.nth ls 4

let get_5th_v2 ls = 
  let rec helper ls i =
    match ls with
    | [] when i < 5 -> 0
    | head :: _ when i = 4 -> head
    | _ :: rest -> helper rest (i + 1) [@@ocaml.warning "-8"] in 
  helper ls 0

let sort_ls ls = List.sort Stdlib.compare ls

(* Exercise: library test *)

(* Exercise: library puzzle *)
let get_last ls = List.rev ls |> fun ls -> List.nth ls 0

let any_zeros (ls: int list): bool = List.exists (fun elem -> elem = 0) ls

(* Exercise: take drop *)
let take n ls = 
  let rec take_aux n ls i acc = 
    if List.is_empty ls || i == n then acc
    else take_aux n (List.tl ls) (i+1) (acc @ [List.hd ls]) in 
    take_aux n ls 0 []

let rec drop n ls = 
  match n with
  | 0 -> ls
  | _ when List.is_empty ls -> ls
  | _ -> drop (n-1) (List.tl ls)

(* Exercise: take drop tail *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
let ( -- ) i j = from i j []

(* let long_list = 0 -- 1_000_000 *)


(* Exercise: unimodal *)
type enumDir = Inc | Dec
let is_unimodal (ls: int list): bool = 
  let rec helper (ls: int list) (direction: enumDir) (prev: int): bool = 
    match (ls, direction) with 
    | ([], _) -> true
    | (head::rest, Inc) when prev <= head -> helper rest direction head
    | (head::rest, Inc) when prev > head -> helper rest Dec head
    | (head::rest, Dec) when prev >= head -> helper rest direction head
    | _ -> false in
  helper ls Inc min_int 

(* Exercise: powerset *)
let powerset ls = 
  let rec helper ls builder result = 
    match ls with 
    | [] -> result @ [builder]
    | head :: rest -> helper rest (builder @ [head]) (result @ [builder]) in 
  helper ls [] []

let rec print_int_list = function
  | [] -> ()
  | h :: t -> Printf.printf "%d\n" h; print_int_list t

(* Exercise: student *)
type struct_student = {first_name : string; last_name : string; gpa : float}
let student (student: struct_student): struct_student = student
let get_student_name (student: struct_student): string * string = (student.first_name, student.last_name)
let create_student first_name last_name gpa = {first_name = first_name; last_name = last_name; gpa = gpa}

(* Exercise: pokerecord *)
type enumPoketype = Normal | Fire | Water
type strcutPokemon = {name: string; hp: int; ptype: enumPoketype}
let charizard = {name = "charizard"; hp = 78; ptype = Fire}
let squirtle = {name = "charizard"; hp = 44; ptype = Water}
let meowth = {name = "meowth"; hp = 40; ptype = Normal}


(* Exercise: safe hd and tl *)
let safe_hd (ls: 'a list ): 'a option = 
  match ls with 
  | [] -> None
  | head :: _ -> Some head

let rec safe_tl (ls: 'a list): 'a list option = 
  match ls with
  | [] -> None
  | _ :: rest -> safe_tl rest

(* Exercise: pokefun *)
let max_hp (ls: strcutPokemon list): strcutPokemon option = 
  let rec helper (ls: strcutPokemon list) (max_hp_pokimon: strcutPokemon option) = 
    match ls, max_hp_pokimon with 
    | [], option_pokimon -> option_pokimon
    | head :: rest, None -> helper rest (Some head)
    | head :: rest, Some pokimon when pokimon.hp < head.hp -> helper rest (Some head)
    | _ :: rest, some_pokimon -> helper rest some_pokimon in 
  helper ls None

(* Exercise: date before *)
type date = int * int * int
let is_before (first_date: date) (second_date: date): bool = 
  match first_date, second_date with 
  | (first_year, first_month, first_day), (second_year, second_month, second_day) -> 
    first_year < second_year && first_month < second_month && first_day < second_day

(* Exercise: earliest date *)
let earliest (date_ls: date list): date option = 
  let rec helper (date_ls: date list) (result: date option): date option = 
    match date_ls, result with 
    | [], option_date -> option_date
    | head :: rest, None -> helper rest (Some head)
    | head :: rest, Some date when is_before head date -> helper rest (Some head)
    | _ :: rest, _ -> helper rest result in
  helper date_ls None

(* Exercise: assoc list *)
let insert k v lst = (k, v) :: lst

let rec lookup k = function
| [] -> None
| (k', v) :: t -> if k = k' then Some v else lookup k t

let assoc_ls = insert 1 "one" [] |> insert 2 "two" |> insert 3 "three"

(* Exercise: cards *)
type enumSuit = Club | Diamond | Heart | Spade
type enumRank = Number of int | Jack | Queen | King | Ace
type structCard = {suit: enumSuit; rank: enumRank}
let cards = [{suit = Club; rank=Ace};{suit = Heart; rank=Queen};{suit = Diamond; rank=Number 2};{suit = Spade; rank=Number 7}]

(* Exercise: matching *)
let ls1 = [None; Some 1]
let ls2 = [None, None, None]
let ls3 = [None, None]
let ls4 = [None, None]
let ls5 = []

(* Exercise: quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign = if x = 0 then Zero else if x > 0 then Pos else Neg

let quadrant (x, y: int*int): quad option = 
  match sign x, sign y with
    | Pos, Pos -> Some I
    | Neg, Pos -> Some II
    | Neg, Neg -> Some III
    | Pos, Neg -> Some IV
    | _ -> None

let quadrant_when (x, y: int*int): quad option =
  match x, y with
    | (x, y) when x > 0 && y > 0 -> Some I
    | (x, y) when x < 0 && y > 0 -> Some II
    | (x, y) when x < 0 && y < 0 -> Some III
    | (x, y) when x > 0 && y < 0 -> Some IV
    | _ -> None

(* Exercise: depth *)
type 'a tree = None | TreeNode of 'a * 'a tree * 'a tree
let rec depth (t: 'a tree): int = 
  match t with 
  | None -> 0
  | TreeNode (_, left, right) -> 1 + max (depth left) (depth right)

let tree1 = TreeNode(10, TreeNode(5, None, None), TreeNode(4, None, TreeNode(9, None, TreeNode(2, None, None))))

(* Exercise: shape *)
let rec same_shape (t1: 'a tree) (t2: 'a tree): bool = 
  match t1, t2 with
  | None, None -> true
  | TreeNode(v1, left1, right1), TreeNode(v2, left2, right2) -> (same_shape left1 left2) && (same_shape right1 right2)
  | _ -> false

let tree2 = TreeNode(8, TreeNode(9, None, None), TreeNode(3, None, TreeNode(2, None, TreeNode(1, None, None))))
let tree3 = TreeNode(8, TreeNode(9, None, None), TreeNode(3, None, TreeNode(2, None, None)))

(* Exercise: list max exn *)
let list_max (ls: int list): int = 
  let rec helper (ls: int list) (result: int option): int = 
    match ls, result with 
    | [], None -> failwith "list_max"
    | [], Some max_value -> max_value
    | head :: rest, None -> helper rest (Some head)
    | head :: rest, Some value -> helper rest (Some (if head > value then head else value)) in 
  helper ls None

(* Exercise: list max exn string *)
let list_max_string (ls: int list): string = 
  match ls with 
  | [] -> "empty"
  | _ -> list_max ls |> string_of_int

(* Exercise: list max exn ounit *)

(* Exercise: is_bst *)
let is_bst (tree: int tree): bool = 
  let rec helper (root: int tree) (lower_bound: int) (upper_bound: int): bool = 
    match root with 
    | None -> true
    | TreeNode(root_val, _, _) when not (lower_bound < root_val && root_val < upper_bound) -> false
    | TreeNode(root_val, left_node, right_node) -> helper left_node lower_bound root_val && helper right_node root_val upper_bound in
  helper tree min_int max_int

let binary_tree = TreeNode(5, TreeNode(1, None, None), TreeNode(4, TreeNode(3, None, None), TreeNode(6, None, None)))
let binary_tree2 = TreeNode(5, TreeNode(1, None, None), TreeNode(7, TreeNode(6, None, None), TreeNode(10, None, None)))

(* inorder travserse validate *)
(* let is_bst2 tree = 
  let rec inorder root prev = 
    match root, prev with 
    | *)

(* Exercise: quadrant poly *)
let quadrant_poly (x, y: int*int) = 
  match x, y with
  | (x, y) when x > 0 && y > 0 -> Some `QuadI
  | (x, y) when x < 0 && y > 0 -> Some `QuadII
  | (x, y) when x < 0 && y < 0 -> Some `QuadIII
  | (x, y) when x > 0 && y < 0 -> Some `QuadIV
  | _ -> None