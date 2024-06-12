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