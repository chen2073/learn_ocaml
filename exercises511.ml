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

(* Exercise: fraction *)
module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0. *)
  type t

  (** [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module TupleFraction: Fraction = struct
  type t = (int * int)

  let make n d = (n, d)

  let numerator (n, d) = n

  let denominator (n, d) = d

  let to_string (n ,d) = Printf.sprintf "%d / %d\n" n d

  let to_float (n, d) = (float_of_int n) /. (float_of_int d)

  let add (n, d) (n', d') = 
    match d = d' with 
    | true -> (n+n', d)
    | false -> (n * d' + n' * d, d * d')

  let mul (n, d) (n', d') = (n * n', d * d')
end

(* Exercise: fraction reduced *)
(** [gcd x y] is the greatest common divisor of [x] and [y].
    Requires: [x] and [y] are positive. *)
let rec gcf x y =
  if x = 0 then y
  else if (x < y) then gcf (y - x) x
  else gcf y (x - y)

module TupleFractionSimplified = struct
  include TupleFraction
  let make n d = 
    let greatest_common_factor = gcf n d in
      (n / greatest_common_factor, d / greatest_common_factor)

  let add (n, d) (n', d') = 
    match d = d' with 
    | true -> make (n+n') d
    | false -> make ( n * d' + n' * d ) ( d * d')

  let mul (n, d) (n', d') =  make (n * n') (d * d')
end