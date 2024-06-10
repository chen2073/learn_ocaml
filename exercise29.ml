(* ex: values 
int : 42
string : CS 3110
*)

(* ex: operators *)
42 * 10;;
3.14 *. 2.0;;
let rec pow_aux n exp acc = if exp = 0 then acc else pow_aux n (exp-1) (acc *. n)
let pow n exp = pow_aux n exp 1.
let seventh_pow n = pow n 7
let _ = seventh_pow 4.2

(* ex: equality *)
let _ = 42 = 42
let _ = compare "hi" "hi" (* int : 0 *)
let _ = "hi" == "hi" (* bool : false *)
(* referential equality *)

(* ex: assert *)
(* unit: () *)
(* exception *)
let _ = assert (2110 != 3110)

(* ex: if *)
let _ = if 2 > 1 then 42 else 7

(* ex: double fun *)
let double x = x * 2
let _ = double 7 |> fun x -> x = 10 |> fun x -> assert x
