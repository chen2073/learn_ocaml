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