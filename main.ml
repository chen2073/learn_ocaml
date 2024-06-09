let add x y = x + y

let incr x = add x 1

let decide x y = if x + y > 10 then true else false

(* lexical scope *)
(* let a = 10 in let b = 11 in a + b *)

(* function application *)
(* (fun x -> x + 1) 3 *)

(* pipeline *)
(* 3 |> incr |> fun x -> x * x *)

(* polymorphic function *)
let add (x : float) (y : float) = x +. y