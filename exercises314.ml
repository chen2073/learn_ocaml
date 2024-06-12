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