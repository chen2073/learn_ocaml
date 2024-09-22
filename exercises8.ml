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
  inspect table;;


(* Exercise: functorized BST *)

module type Comparable = sig
  type t
  val compare: t -> t -> int
  (* 0 -> equal, 1 -> t1 > t2, -1 -> t1 < t2 *)
end

module BstSet (ComparableVal: Comparable) = struct
  type 'a tree = None | Node of 'a * 'a tree * 'a tree 

  let rec mem x = function
    | None -> false
    | Node (value, left, right) ->
      if compare x value < 0 then mem x left
      else if compare x value > 0 then mem x right
      else true

  let rec insert x = function
    | None -> Node (x, None, None)
    | Node (value, left, right) as node ->
      if compare x value < 0 then Node (value, insert x left, right)
      else if compare x value > 0 then Node (value, left, insert x right)
      else node
end

module MyInt: Comparable = struct
  type t = int
  let compare int1 int2 = 
    if int1 > int2 then 1
    else if int1 < int2 then -1
    else 0
end

module IntSet = BstSet (MyInt)

(* Exercise: efficient traversal *)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec preorder = function
  | Leaf -> []
  | Node (l,v,r) -> [v] @ preorder l @ preorder r

let rec inorder = function
  | Leaf -> []
  | Node (l,v,r) ->  inorder l @ [v] @ inorder r

let rec postorder = function
  | Leaf -> []
  | Node (l,v,r) ->  postorder l @ postorder r @ [v]

let t =
  Node(Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
       4,
       Node(Node(Leaf, 5, Leaf), 6, Node(Leaf, 7, Leaf)))

(*
  t is
        4
      /   \
     2     6
    / \   / \
   1   3 5   7
*)

let () = assert (preorder t  = [4;2;1;3;6;5;7])
let () = assert (inorder t   = [1;2;3;4;5;6;7])
let () = assert (postorder t = [1;3;2;5;7;6;4])

let preorder_eff tree_node =
  let rec preorder_eff' stack result = 
    match stack with 
    | [] -> List.rev result
    | head :: stack' -> 
      match head with 
      | Leaf -> preorder_eff' stack' result
      | Node (left, value, right) -> 
        let result' = value :: result in 
        match left, right with
        | Leaf, Leaf -> preorder_eff' stack' result'
        | left, Leaf -> preorder_eff' (left :: stack') result'
        | Leaf, right -> preorder_eff' (right :: stack') result'
        | left, right -> preorder_eff' (left :: right :: stack') result' in 
  preorder_eff' (tree_node :: []) []

let () = assert (preorder_eff t = [4;2;1;3;6;5;7])

let inorder_eff tree_node = 
  let rec inorder_eff' cur_node stack result = 
    match cur_node with
    | Node (left, value, right) -> inorder_eff' left (cur_node :: stack) result
    | Leaf -> 
      match stack with 
      | [] -> List.rev result
      | Node (left, value, right) :: stack -> inorder_eff' right stack (value :: result)
      | _ -> failwith "impossible" in 
  inorder_eff' tree_node [] []

let () = assert (inorder_eff t = [1;2;3;4;5;6;7])

let postorder_eff tree_node = 
  let rec postorder_eff' node result = 
    match node with 
    | Leaf -> result
    | Node (left, value, right) -> value :: (postorder_eff' right (postorder_eff' left result)) in 
  postorder_eff' tree_node [] |> List.rev

let () = assert (postorder_eff t = [1;3;2;5;7;6;4])

(* Exercise: RB draw complete *)

(* Exercise: RB draw insert *)

(* Exercise: pow2 *)
type 'a sequence = Cons of 'a * (unit -> 'a sequence)
let rec pow2 n = Cons (n, fun () -> pow2 (n * 2))

let rec even n = Cons (n, fun () -> even (n + 2))

let rec alphabet letter = Cons (letter, fun () -> alphabet (
  if Char.code letter > 96 && Char.code letter < 122 
    then Char.code letter |> (+) 1 |> Char.chr
  else 'a'
))

type coinSide = Head | Tail
let rec head_or_tail side = Cons (side, fun () -> 
  head_or_tail (if side = Head 
    then Tail 
  else Head 
))

(* Exercise: nth *)
let rec nth seq n = 
  let Cons (head, tail) = seq in
  match n with 
  | 0 -> head
  | _ -> nth (tail ()) (n-1)

(* Exercise: hd tl *)

(* Exercise: filter *)
let rec filter pred seq = 
  let Cons (head, tail) = seq in 
  if pred head 
    then Cons (head, fun () -> filter pred (tail ()))
  else
    filter pred (tail ())

(* Exercise: interleave *)
let rec interleave seq1 seq2 = 
  let Cons (head1, tail1) = seq1 in 
  let Cons (head2, tail2) = seq2 in 
  Cons (head1, fun () -> Cons (head2, fun () -> interleave (tail1 ()) (tail2 ()) ))

(* Exercise: sift *)
let rec sift n seq = filter (fun x -> x mod n <> 0) seq

let rec nat n = Cons (n, fun () -> nat (n+1)) 

let hd (Cons (h, _)) = h

(* [tl s] is the tail of [s] *)
let tl (Cons (_, tf)) = tf ()

let rec take n s =
  if n=0 then []
  else hd s :: take (n-1) (tl s)

(* Exercise: primes *) 

let prime =
  let rec is_prime n i = 
    if n < 2 
      then false
    else if n = i 
      then true
    else if n mod i = 0
      then false
    else 
      is_prime n (i+1) in
  let rec prime' n = 
  match is_prime n 2 with
  | true -> Cons (n, fun () -> prime' (n+1))
  | false -> prime' (n+1) in 
  prime' 2

let pow x k = 
  let rec pow' x' k' acc = 
    if k' == 0 
      then acc
    else
      pow' x' (k'-1) (x' *. acc) in
  pow' x k 1.0

let rec factorial n = 
  if n <= 1
    then 1
  else if n = 2
    then 2
  else
    n * factorial (n-1)

(* Exercise: approximately e *)
let e_terms x =
  let k_th_term x' k' = (pow x' k') /. float_of_int (factorial k') in
  let rec e_terms' x'' nth = Cons (k_th_term x'' nth, fun () -> e_terms' x'' (nth+1)) in 
  e_terms' x 0

let total seq = 
  let rec total' acc seq' = 
    let Cons (hd, tl) = seq' in
    Cons (hd+.acc, fun () -> total' (hd+.acc) (tl ())) in
  total' 0.0 seq

let abs_diff float1 float2 = 
  if (float1 -. float2) < 0.0
    then float2 -. float1
  else
    float1 -. float2

let within_generic (differentiator: (float -> float -> float)) (eps: float) (Cons (hd, tl): float sequence): float = 
  let rec within' prev_elem (Cons (cur_elem, next_elem)) = 
    if (differentiator prev_elem cur_elem) < eps
      then cur_elem
    else
      within' cur_elem (next_elem ()) in
  within' hd (tl ())

let within = within_generic abs_diff

let e x eps = e_terms x |> total |> within eps

(* Exercise: better e *)

(* 
  x = 1
  numerator power: 0, x ^ 0 -> 1, denominator: factorial 0 * 1-> 1, approximate: 1 
  numerator power: 1, x -> x, denominator: factorial 1 * 1 -> 1, approximate: 1
  numerator power: 2, x * x -> x^2, denominator: factorial 2 * 1 -> 2, approximate: 0.5
  numerator power: 3, x * x * x -> x^3, denominator: factorial 3 * 2 -> 6, approximate: 1.66666
  numerator power: 3, x * x * x * x-> x^4, denominator: factorial 4 * 6 -> 24, approximate: 0.041666
  *)

let e_terms_eff (x: float): float sequence = 
  let rec e_terms_eff' nth numerator fact_n fact_prod approx = 
    (* first term -> zero *)
    if nth = 0
      then Cons (1.0, fun () -> e_terms_eff' 1 x 1.0 1.0 0.0 )
    (* 2nd term *)
    else if nth = 1
      then Cons (2.0, fun () -> e_terms_eff' 2 (x*.x) 2.0 1.0 2.0 )
    (* 3rd and so forth *)
    else
      let cur_nth_approx = (numerator *. x) /. (fact_n *. fact_prod) in
      let total_approx = approx +. cur_nth_approx in
      Cons (total_approx, fun () -> e_terms_eff' (nth+1) (numerator *. x) (fact_n+.1.0) (fact_n *. fact_prod) total_approx ) in
  e_terms_eff' 0 0.0 0.0 0.0 0.0

let relative_distance a b = (a -. b) /. (Float.min_num a b) |> Float.abs

let within_rel_dis = within_generic relative_distance

let e_eff x eps = epsilon_float |> within_rel_dis


(* Exercise: different sequence rep *)
module MySequence = struct

  type 'a sequence = Cons of (unit -> 'a * 'a sequence)

  let hd (seq: 'a sequence): 'a = 
    let Cons (seq_func) = seq in
    let head, _ = seq_func () in
    head

  let tl (seq: 'a sequence): 'a sequence = 
    let Cons(seq_func) = seq in 
    let _, tail = seq_func () in 
    tail

  let nat = 
    let rec nat' a = Cons (fun () -> (a, nat' (a+1))) in 
    nat' 0

  let rec map (f: 'a -> 'b) (seq: 'a sequence): 'b sequence = 
    let Cons (seq_func) = seq in 
    let head, tail = seq_func () in 
    Cons (fun () -> (f head, map f tail))

end

(* this sequence is lazier because it doesn't even evaluate head and tail when calling next from sequence until you evaluate it *)

(* Exercise: lazy hello *)
let hello =
  lazy (print_endline "Hello lazy world")

(* Exercise: lazy and *)
let ( &&& ) (lb1: bool Lazy.t) (lb2: bool Lazy.t): bool =
  if not (Lazy.force lb1)
    then false
  else
    Lazy.force lb2

(* Exercise: lazy sequence *)
module LazySequence = struct
  type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t

  let rec map (f: 'a -> 'b) (lazy_seq: 'a lazysequence): 'b lazysequence = 
    let Cons (head, tail) = lazy_seq in
    let force_tail = Lazy.force tail in
    Cons (f head,  lazy (map f force_tail))

  let rec filter (pred: 'a -> bool) (lazy_seq: 'a lazysequence): 'a lazysequence =
    let Cons (head, tail) = lazy_seq in
    let force_tail = Lazy.force tail in
    if pred head
      then Cons (head, lazy (filter pred force_tail))
    else
      filter pred force_tail
end
