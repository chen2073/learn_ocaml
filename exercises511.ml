(* Exercise: complex synonym  *)

module type ComplexSig = sig
  type t = float * float
  val zero : t
  val add : t -> t -> t
end

(* Exercise: complex encapsulation *)
(* remove zero from the structure

no identity element

remove add from the signature

no operation on the type float * float

change zero in the structure to let zero = 0, 0  

mismatched type with float * float and int * int *)

(* Exercise: big list queue *)
(** Creates a ListQueue filled with [n] elements. *)
module ListQueue = struct
  let empty = []

  let enqueue (n: int) (q: 'a list) = 
    match n with
    | 0 -> q
    | _ -> q @ [n]
end


let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

(* Exercise: queue efficiency *)
module BatchedQueue = struct
  let empty = []

  let enqueue n q = 
    match n with 
    | 0 -> q
    | 1 -> List.rev (0 :: q)
    | _ -> n :: q
end


let fill_batchedqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (BatchedQueue.enqueue n q) in
  loop n BatchedQueue.empty

(* Exercise: queue efficiency *)
(* listqueue: for each append operation, memory address locator has 
  to traverse through the list to get to the end then apply append operation, which results in O(n^2) *)

(* bachedqueue: for each append operation, append operator is O(1) and List.rev is O(1), 
  according to doc so overall complexity is O(1) *)

(* Exercise: binary search tree map *)
module type Map = sig
  type ('k, 'v) t
  val empty  : ('k, 'v) t
  val insert : 'k -> 'v -> ('k,'v) t -> ('k,'v) t
  val lookup  : 'k -> ('k,'v) t -> 'v
end

module BstMap: Map = struct
  type 'a tree = 
  | None
  | Node of 'a * 'a tree * 'a tree

  type ('k, 'v) t = ('k * 'v) tree

  let empty = None

  let rec insert k v t = 
    match t with 
    | None -> Node((k, v), None, None)
    | Node ((k',v'), l, r) -> 
      match k with 
      | _ when k = k' -> Node((k, v), l, r)
      | _ when k < k' -> Node((k', v'), insert k v l, r)
      | _ when k > k' -> Node((k', v'), l, insert k v r)
      [@@ocaml.warning "-8"]

  let rec lookup k t = 
    match t with 
    | None -> failwith "key not found"
    | Node ((k',v'), l, r) -> 
      match k with 
      | _ when k = k' -> v'
      | _ when k < k' -> lookup k l
      | _ when k > k' -> lookup k r
      [@@ocaml.warning "-8"]

end