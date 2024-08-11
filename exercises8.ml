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

(* Exercise: hashtbl load factor *)
let load_factor hashtable = 
  let stat = Hashtbl.stats hashtable in 
  float_of_int stat.num_bindings /. float_of_int stat.num_buckets

let _ = Hashtbl.stats tab
let _ = Hashtbl.add tab 100 "100"
let _ = Hashtbl.stats tab
let _ = Hashtbl.add tab 200 "200"
let _ = Hashtbl.stats tab

(* Exercise: functorial interface *)
module StringHashTableKey = struct
  type t = string
  let equal str1 str2 = str1 = str2
  let hash str = String.fold_left (fun acc char -> acc * 10 + Char.code char) 0 str
end

module StringHashTable = Hashtbl.Make(StringHashTableKey)

let ht = StringHashTable.create 5
let _ = StringHashTable.add ht "Abc" "abc"

(* Exercise: equals and hash *)
(* it tell the code how to hash and how to compare keys which are essential operations for hashtable insertion *)

(* Exercise: bad hash *)
module BadHashTableKey = struct
  type t = int
  let equal a b = a = b
  let hash _ = 3
end

module BadHashTable = Hashtbl.Make(BadHashTableKey)

let _ = 
  let ht1 = BadHashTable.create 10 in
  BadHashTable.stats ht1;
  BadHashTable.add ht1 1 "1";
  BadHashTable.stats ht1;
  BadHashTable.add ht1 2 "2";
  BadHashTable.stats ht1;
  BadHashTable.add ht1 3 "3";;


module HashTableLinearProbe: (sig
  type ('k, 'v) t
  val find: ('k, 'v) t -> 'k -> 'v option
  val insert: ('k, 'v) t -> 'k -> 'v -> unit
  val remove: ('k, 'v) t -> 'k -> unit
  val resize: ('k, 'v) t -> unit
end) = struct

  type ('k, 'v) binding = {key: 'k; value: 'v; mutable isDeleted: bool}

  type ('k, 'v) t = {
    hash_function: 'k -> int;
    mutable bindings: int;
    (* buckets = array length *)
    mutable buckets: int;
    mutable array_bindings: ('k,'v) binding option array
  } 

  let find hashtable key = 
    let hash_index = hashtable.hash_function key in
    let rec linear_search index = 
      (* defensive guard for preventing dead loop in linear search *)
      if index > hash_index && index mod hashtable.buckets = index 
        then failwith "dead loop in linear search for find"
      else 
        match hashtable.array_bindings.(index mod hashtable.buckets) with 
        | None -> None
        | Some binding -> 
          match binding.key = key with
          | true -> Some binding.value
          | false -> linear_search (index+1) in
        linear_search hash_index

  let insert hashtable key value = 
    let hash_index = hashtable.hash_function key in
    let rec linear_search index = 
      if index > hash_index && index mod hashtable.buckets = index 
        then failwith "dead loop in linear search for insert"
      else
        match hashtable.array_bindings.(index mod hashtable.buckets) with
        | None -> hashtable.array_bindings.(index mod hashtable.buckets) <- Some {key=key; value=value; isDeleted=false}
        | Some _ -> linear_search (index+1) in 
        linear_search hash_index;
        (* impreative increment binding count *)
        hashtable.bindings <- hashtable.bindings + 1

  let remove hashtable key = 
    let hash_index = hashtable.hash_function key in
    let rec linear_search index = 
      if index > hash_index && index mod hashtable.buckets = index 
        then failwith "dead loop in linear search for remove"
      else
        match hashtable.array_bindings.(index mod hashtable.buckets) with
        | None -> linear_search (index+1)
        | Some bindings -> 
          match bindings.key = key with 
          | true -> bindings.isDeleted <- true
          | false -> linear_search (index+1) in 
        linear_search hash_index

  let get_load_factor hashtable = float_of_int hashtable.bindings /. float_of_int hashtable.buckets

  let resize hashtable = ()
end