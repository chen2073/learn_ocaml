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
let _ = double 7 |> fun x -> x = 10

(* ex: more fun *)
let cube_float n = pow n 3
let get_sign n = 
  if n = 0 then 0
  else if n < 0 then -1
  else 1
let get_area r = pow r 2 *. 3.14
let test_get_area r true_area = assert ((get_area r) = true_area)
let _ = test_get_area 5. 78.5
(* let _ = test_get_area 5.2356745 86.0745826556 *)

(* ex: rms *)
let rms x y = 
  let x_square = x *. x in 
  let y_square = y *. y in 
  let sum_suqare = x_square +. y_square in 
  let half_sum_square = sum_suqare /. 2. in 
  let sqrt_half_sum_square = sqrt half_sum_square in
  sqrt_half_sum_square

(* ex: date fun *)
let date_fun (d: int) (m: string) : bool = 
  match m with
    | "Feb" -> 0 < d && d <= 28
    | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> 0 < d && d <= 31
    | "Apr" | "Jun" | "Sept" | "Nov" -> 0 < d && d <= 30
    | _ -> invalid_arg "m"
