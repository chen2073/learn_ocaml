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
  (* BadHashTable.stats ht1 ;  *)
  BadHashTable.add ht1 1 "1";
  (* BadHashTable.stats ht1; *)
  BadHashTable.add ht1 2 "2";
  (* BadHashTable.stats ht1; *)
  BadHashTable.add ht1 3 "3" ;;


module HashTableLinearProbe: (sig
  type ('k, 'v) t
  val inspect: ('k, 'v) t -> unit
  val create: ('k -> int) -> ('k -> int) -> ('v -> string) -> ('k, 'v) t
  val find: ('k, 'v) t -> 'k -> 'v option
  val insert: ('k, 'v) t -> 'k -> 'v -> unit
  val remove: ('k, 'v) t -> 'k -> unit
  val resize: ('k, 'v) t -> unit
end) = struct

  type ('a, 'b) binding = {key: 'a; value: 'b; mutable isDeleted: bool}

  type ('k, 'v) t = {
    hash_function: 'k -> int;
    print_key: 'k -> int;
    print_value: 'v -> string;
    (* bindings are active valid count of current bindings *)
    mutable bindings: int;
    (* deleted are count of virtually deleted bindings *)
    mutable deleted: int;
    (* buckets is array length *)
    mutable buckets: int;
    mutable array_bindings: ('k, 'v) binding option array
  } 

  type resizeOption = ResizeUp | ResizeDown

  let create hash_function print_key print_value = {
    hash_function = hash_function;
    print_key = print_key;
    print_value = print_value;
    bindings = 0;
    deleted = 0;
    buckets = 10;
    array_bindings = Array.make 10 None
  }

  let inspect hashtable = Printf.printf
    "stats: (bindings: %d, deleted: %d, buckets: %d)\n" hashtable.bindings hashtable.deleted hashtable.buckets;
    for i = 0 to Array.length hashtable.array_bindings - 1 do
      Printf.printf "index: %d; binding: %s\n" i 
      (match hashtable.array_bindings.(i) with
      | None -> "None"
      | Some binding -> 
        Printf.sprintf "(key %d:, value: %s, isDeleted: %s)" 
        (hashtable.print_key binding.key) 
        (hashtable.print_value binding.value) 
        (string_of_bool binding.isDeleted)
      )
    done;;
    
  (* get_hash_index_fit: fit hashed index within bucket size*)
  let get_hash_index_fit (hashtable: ('k, 'v) t) (hash_index_raw: int): int = hash_index_raw mod hashtable.buckets
  
  (* linear_probe_insert: insert key value in hashtable using linear probe (next available bucket) *)
  let linear_probe_insert (hashtable: ('k, 'v) t) (key: 'k) (value: 'v): unit = 
    let hash_index_fit = get_hash_index_fit hashtable (hashtable.hash_function key) in
    let rec linear_search_empty index = 
      match hashtable.array_bindings.(index) with
      | Some _ -> linear_search_empty (index + 1)
      | None -> hashtable.array_bindings.(index) <- Some {key = key; value = value; isDeleted = false} in 
    linear_search_empty hash_index_fit
  
  (* remap: double or half array binding size and remap old array items to new array *)
  let remap_array_bindings (hashtable: ('k, 'v) t) (resize_option: resizeOption): unit = 
    let old_array_bindings = hashtable.array_bindings in
    let new_array_bindings = Array.make (match resize_option with 
      | ResizeUp -> hashtable.buckets * 2 
      | ResizeDown -> hashtable.buckets / 2) 
      None in
    (* assign double sized array to hashtable and reset binding and deleted count to 0 *)
    hashtable.array_bindings <- new_array_bindings;
    hashtable.buckets <- Array.length new_array_bindings;
    hashtable.bindings <- 0;
    hashtable.deleted <- 0;
    (* remap old binding to new array and increament binding count *)
    Array.iter (fun binding -> 
      match binding with
      | Some {key; value; isDeleted} when isDeleted = false -> 
        linear_probe_insert hashtable key value; 
        hashtable.bindings <- hashtable.bindings + 1
      | _ -> ()
      ) old_array_bindings;;
    
  let find hashtable target_key = 
    let hash_index_raw = hashtable.hash_function target_key in
    let rec linear_search index = 
      let hash_index_fit = get_hash_index_fit hashtable index in
      (* defensive guard for preventing dead loop in linear search *)
      if index > hash_index_raw && hash_index_fit = index 
        then failwith "dead loop in linear search for find"
      else
        match hashtable.array_bindings.(hash_index_fit) with 
        | None -> None
        | Some binding -> 
          if binding.key = target_key && not binding.isDeleted then Some binding.value 
          else linear_search (index+1) in
    linear_search hash_index_raw;;

  let insert hashtable key value = 
    (* determine if resize needed before insert *)
    if (float_of_int (hashtable.bindings+1) +. float_of_int hashtable.deleted) /. float_of_int hashtable.buckets > 0.5 
      then remap_array_bindings hashtable ResizeUp;
    linear_probe_insert hashtable key value;
    hashtable.bindings <- hashtable.bindings + 1;;

  let remove hashtable target_key = 
    let hash_index_raw = hashtable.hash_function target_key in
    let rec linear_search index = 
      let hash_index_fit = get_hash_index_fit hashtable index in
      if index > hash_index_raw && hash_index_fit = index 
        then failwith "dead loop in linear search for find"
      else 
        match hashtable.array_bindings.(hash_index_fit) with 
        | Some binding when binding.isDeleted = false -> 
          binding.isDeleted <- true
        | _ -> linear_search (index+1) in
    linear_search hash_index_raw;
    hashtable.deleted <- hashtable.deleted + 1;
    if (float_of_int hashtable.bindings -. float_of_int hashtable.deleted) /. float_of_int hashtable.buckets < 0.125
      then remap_array_bindings hashtable ResizeDown;;
    
  let resize hashtable = 
    if (float_of_int hashtable.bindings +. float_of_int hashtable.deleted) /. float_of_int hashtable.buckets > 0.5 
      then remap_array_bindings hashtable ResizeUp
    else if (float_of_int hashtable.bindings -. float_of_int hashtable.deleted) /. float_of_int hashtable.buckets < 0.125
      then remap_array_bindings hashtable ResizeDown;;
end

let () = 
  let open HashTableLinearProbe in
  let print_key = (fun key -> key) in 
  let hash_key = print_key in 
  let print_value = (fun value -> value) in 
  let table = create hash_key print_key print_value in 
  Printf.printf "\n\n\n";
  
  insert table 0 "zero";
  insert table 1 "one";
  insert table 2 "two";
  insert table 3 "three";
  insert table 11 "eleven";
  insert table 4 "four";
 
  (* inspect table;
  inspect_array_bindings table print_key print_value; *)

  insert table 5 "five";

  inspect table;
  find table 1 |> Option.get |> Printf.printf "value: %s\n";
  find table 11 |> Option.get |> Printf.printf "value: %s\n";

  remove table 5;
  remove table 4;
  remove table 3;
  remove table 2;
  remove table 0;
  inspect table;