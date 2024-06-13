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