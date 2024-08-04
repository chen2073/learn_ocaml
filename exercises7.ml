(* Exercise: mutable fields *)

type studentRecord = { name: string; mutable gpa: float }

let alice = {name="Alice"; gpa=3.7}

let () = alice.gpa <- 4.0

let a = ref true
let b = ref [1;2;3;4]

let c = [ref 1; ref 2; ref 3]


(* Exercise: inc fun *)
let inc = ref (fun x -> x + 1)

(* let inc_rec accumulator = if accumulator = 3110 then accumulator else !inc (accumulator+1)

inc := inc_rec *)

(* Exercise: addition assignment *)
let ( +:= ) x y = x := !x + y

let x = ref 0;;
x +:= 3110;;
!x;;

(* Exercise: physical equality *)
let x = ref 0
let y = x
let z = ref 0

(* # x == y;; true
# x == z;; false
# x = y;; true
# x = z;; true
# x := 1;; unit
# x = y;; true
# x = z;; false *)

(* Exercise: norm *)
let normalize (arr: float array): unit = 
  let square x = x *. x in
  let squared_array = Array.map square arr in
  let sum_squared_array = Array.fold_left ( +. ) 0. squared_array in
  let sqrt_sum_squared_array = Float.sqrt sum_squared_array in
  Array.iteri (fun i elem -> arr.(i) <- elem /. sqrt_sum_squared_array) arr

let arr = [| 5.; 3.; 6.; 3.; 9.; 4.; |];;
normalize arr;;
arr;;

(* Exercise: norm loop *)
let normalize_loop (arr: float array): unit = 
  let square x = x *. x in
  let squared_array = Array.map square arr in
  let sum_squared_array = Array.fold_left ( +. ) 0. squared_array in
  let sqrt_sum_squared_array = Float.sqrt sum_squared_array in
  for i = 0 to Array.length arr do 
    arr.(i) <- arr.(i) /. sqrt_sum_squared_array
  done

let arr' = [| 5.; 3.; 6.; 3.; 9.; 4.; |];;
normalize arr';;
arr';;

(* Exercise: init matrix *)

let init_matrix n o f =