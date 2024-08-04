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

(* Exercise: make char map *)
module CharMap = Map.Make(Char)
(* module CharMap :
  sig
    type key = char
    type 'a t = 'a Map.Make(Char).t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end *)

  (* empty: empty map with char type as key *)
  (* add: add key value to the map with key type Map.Make(Char).t and value type 'a *)
  (* remove: remove map's key value by key *)

(* Exercise: char ordered *)
(* char module implments the compare function of two chars *)

(* Exercise: use char map *)
let m = CharMap.empty 
  |> CharMap.add 'A' "Alpha"
  |> CharMap.add 'E' "Echo"
  |> CharMap.add 'S' "Sierra"
  |> CharMap.add 'V' "Victor"

let e_value = CharMap.find 'E' m

let a_is_bound = m |> CharMap.remove 'A' |> CharMap.mem 'A'

let assoc_ls = CharMap.bindings m

(* Exercise: bindings *)
(* all the same *)

(* Exercise: date order *)
type date = {month : int; day : int}

module Date = struct
  type t = date
  let compare {month = month1; day = day1} {month = month2; day = day2} = 
    match month1 - month2 with
    | 0 -> day1 - day2
    | c -> c
end

(* Exercise: calendar *)
module DateMap = Map.Make(Date)

type calendar = string DateMap.t

let c: calendar = DateMap.empty 
  |> DateMap.add {month=3; day=12} "Amy's birthday"
  |> DateMap.add {month=2; day=18} "Chinese new year"

let print_calendar c = 
  let month_int_to_string month = 
    match month with
    | 1 -> "Jan"
    | 2 -> "Feb"
    | 3 -> "Mar"
    | 4 -> "Apr" 
    | 5 -> "May" 
    | 6 -> "Jun" 
    | 7 -> "Jul" 
    | 8 -> "Aug"
    | 9 -> "Sep"
    | 10 -> "Oct"
    | 11 -> "Nov" 
    | 12 -> "Dec" 
    | _ -> failwith "invalid month" in
  let print_entry {month=month; day=day} event = 
    Printf.printf "Month: %s, day: %d, event: %s\n" (month_int_to_string month) day event in 
DateMap.iter print_entry c 

(* Exercise: is for *)
let is_for (char_map: string CharMap.t): string CharMap.t = 
  let rephrase key value = Printf.sprintf "%c is for %s" key value in
  CharMap.mapi rephrase char_map

let is_for_test = 
  CharMap.empty 
  |> CharMap.add 'a' "apple"
  |> CharMap.add 'b' "bitch"

(* Exercise: first after *)
let first_after (c: calendar) (d: Date.t): string = 
  let (first_after_key, _) = DateMap.find_first (fun k -> Date.compare k d > 0) c in 
  DateMap.find first_after_key c

(* Exercise: sets *)
module CaseInsesitiveSet = Set.Make(struct
  type t = string
  let compare s1 s2 = 
    String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)
end)

let case_ignore_equal = CaseInsesitiveSet.equal (CaseInsesitiveSet.of_list ["grr"; "argh"]) (CaseInsesitiveSet.of_list ["aRgh"; "GRR"])

(* Exercise: ToString *)
module type ToString = sig
  type t
  val to_string: t -> string
end

(* Exercise: Print *)
module Print (M: ToString) = struct
  let print v = Printf.printf "%s\n" (M.to_string v)
end

(* Exercise: Print Int *)
module MyInt = struct
  type t = int
  let to_string = string_of_int
end

module PrintInt = Print(MyInt)

let _ = PrintInt.print 5

module MyString = struct
  type t = string
  let to_string s = s
end

module PrintString = Print(MyString)

let _ = PrintString.print "abcd"

(* Exercise: Print Reuse *)
(* functor is acting as interface to implment different behaviour based on the types *)

(* Exercise: Print String reuse revisited *)
module StringWithPrint = struct
  include String
  include PrintString
end

(* Exercise: implementation without interface *)

(* Exercise: refactor arith *)