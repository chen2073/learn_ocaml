(* Exercise: complex synonym *)
module type ComplexSig = sig
  type t
  val zero : t
  val add : t -> t -> t
end

(* Exercise: complex encapsulation *)
module Complex : ComplexSig = struct
  type t = float * float
  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
end

(* remove zero from the structure: mismatched signature *)

(* remove add from the signature: nothing happens *)

(* change zero in the structure to let zero = 0, 0: mismatched types *)

(* Exercise: big list queue *)
(** Creates a ListQueue filled with [n] elements. *)
module type ListQueue = sig
  val empty : 'a list
  val enqueue : 'a -> 'a list -> 'a list
end

module ListQueue = struct
  let empty = []
  let enqueue n q = q @ [n]
end

let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

(* 100000 -> signaficant delay; 10000 -> noticible delay *)

(* Exercise: big batched queue *)
module type BatchedQueue = sig
  val empty : 'a list
  val enqueue : 'a -> 'a list -> 'a list
end

module BatchedQueue = struct
  let empty = []
  let enqueue n q = n :: q
end

let fill_batchedqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (BatchedQueue.enqueue n q) in
  loop n BatchedQueue.empty

(* faster than list queue *)

(* Exercise: queue efficiency *)
(* for every n, it will have to traverse from begining of the list to get to the end, then append new n to the end, so the complexity is n * n^2,*)

(* each enqueue operation is O(1), and there are n enqueue operations, so n * O(1) = O(n) *)

(* Exercise: binary search tree map *)
module type Map = sig
  type ('k, 'v) t
  val empty: ('k, 'v) t
  val insert: 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val lookup: 'k -> ('k, 'v) t -> 'v
end

module BstMap: Map = struct
  type 'a bst = None | Node of 'a bst * 'a bst * 'a

  type ('k, 'v) t = ('k, 'v) bst
  
  let empty = None

  let rec insert new_k new_v bst = 
    match bst with 
    | None -> Node (None, None, (k, v))
    | (left, right, (k, v)) -> 
      if new_k > k then insert new_k new_v right
      else insert new_k new_v left

  let rec lookup targetKey bst = 
    match bst with
    | None -> failwith "not found"
    | (_, _, (k, v)) when key = targetKey -> v
    | (left, _, (k, v)) when targetKey < k -> lookup targetKey left
    | (_, right, (k, v)) when targetKey > k -> lookup targetKey left

end