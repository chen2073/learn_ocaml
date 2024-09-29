(* Exercise: add opt *)
module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f =
    match m with
    | Some x -> f x
    | None -> None

end

let add (x: int Maybe.t) (y: int Maybe.t): int Maybe.t = 
  let open Maybe in 
  x >>= fun a -> 
    y >>= fun b ->
      Maybe.return (a + b)

let _ = 
  let open Maybe in 
  let c = add (return 3) (return 4) in
  c >>= fun a -> Printf.printf "%d\n" a; return ()

(* Exercise: fmap and join *)
module type ExtMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
end

module ExtMaybe1 : ExtMonad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f =
    match m with
    | Some x -> f x
    | None -> None

  let ( >>| ) m f = 
  match m with
  | None -> None
  | Some a -> Some (f a)

  let join nested_m = 
    match nested_m with
    | None -> None
    | Some a -> a
end

(* Exercise: fmap and join again *)

module ExtMaybe2 : ExtMonad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f =
    match m with
    | Some x -> f x
    | None -> None

  let ( >>| ) m f = 
  m >>= fun a -> return (f a)

  let join nested_m = 
    nested_m >>= fun m -> m
end

(* Exercise: bind from fmap+join *)

module type FmapJoinMonad = sig
  type 'a t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
  val return : 'a -> 'a t
end

module type BindMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module MakeMonad (M : FmapJoinMonad) : BindMonad = struct
  type 'a t = 'a M.t

  let return = M.return

  let ( >>= ) m f = 
  let open M in
  let nested_m = m >>| f in
    join nested_m
end

(* Exercise: list monad *)

module type ExtMonadList = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
end

module ListMonad : ExtMonadList = struct
  type 'a t = 'a list

  let return a = a :: []

  let ( >>| ) m f = List.map f m

  let join = List.flatten

  let ( >>= ) m f = 
  let nested_ls = List.map f m in
    List.flatten nested_ls
end

(* Exercise: trivial monad laws *)
module Trivial : Monad = struct
  type 'a t = Wrap of 'a
  let return x = Wrap x
  let ( >>= ) (Wrap x) f = f x
end

(* Law 1: return x >>= f behaves the same as f x *)
let inc a = Trivial.return (a + 1)

let law1 = 
  let open Trivial in
  let a = return 2 >>= inc in
  let b = inc 2 in 
  a = b

(* Law 2: m >>= return behaves the same as m *)
let law2 = 
  let open Trivial in 
  let a = return 1 in
  let b = return 1 >>= return in
  a = b

(* Law 3: (m >>= f) >>= g behaves the same as m >>= (fun x -> f x >>= g) *)
(* subtraction is not associative so it's clear in this demo *)
let sub a = Trivial.return (a - 1)
let sub2 a = Trivial.return (a - 2)
let law3 = 
  let open Trivial in 
  let a = (return 5 >>= fun x -> sub x) >>= sub2 in
  let b = return 5 >>= (fun x -> sub x >>= sub2) in
  a = b