(* Exercise: hash insert *)
(* 
  4 -> 4
  8 -> 1
  15 -> 1
  16 -> 2
  23 -> 2
  42 -> 0

  {0: [42], 1: [8, 15], 2: [16, 23], 4: [4]}
*)

(* Exercise: relax bucket RI *)
(* insert：O(1)
   find: O(n) n: number of elements in the bucket 
   remove: O(1) *)

(* Exercise: strengthen bucket RI *)
(* insert：O(log n): sort keys in each bucket
   find: O(n) n: number of elements in the bucket 
   remove: O(1) *)

(* Exercise: hash values *)
let _ = Hashtbl.hash ()
let _ = Hashtbl.hash false
let _ = Hashtbl.hash true
let _ = Hashtbl.hash 0
let _ = Hashtbl.hash 1
let _ = Hashtbl.hash ""
let _ = Hashtbl.hash []

(* Exercise: hashtbl usage *)
let tab = Hashtbl.create 16

let _ = 
  let range i = List.init i succ in
  let ls = range 31 in 
  List.iter (fun i -> Hashtbl.add tab i (string_of_int i)) ls

let _ = Hashtbl.iter (fun key value -> Printf.printf "key: %i, value: %s\n" key value) tab;;

let _ = Hashtbl.find tab 3;;

(* let _ = Hashtbl.find tab 33;; *)

(* Exercise: hashtbl stats *)
let _ = Hashtbl.stats tab;;
(* - : Hashtbl.statistics =
{Hashtbl.num_bindings = 31; num_buckets = 16; max_bucket_length = 4;
 bucket_histogram = [|3; 3; 3; 6; 1|]} *)

 (* Exercise: hashtbl bindings *)
let bindings (hashtable: ('a,'b) Hashtbl.t): ('a * 'b) list = 
  Hashtbl.fold (fun key value acc -> (key, value)::acc) hashtable []

let _ = bindings tab;;