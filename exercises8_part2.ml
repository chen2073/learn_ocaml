module type PROMISE = sig
  type 'a state =
    | Pending
    | Fulfilled of 'a
    | Rejected of exn

  type 'a promise

  type 'a resolver

  (** [make ()] is a new promise and resolver. The promise is pending. *)
  val make : unit -> 'a promise * 'a resolver

  (** [return x] is a new promise that is already fulfilled with value
      [x]. *)
  val return : 'a -> 'a promise

  (** [state p] is the state of the promise. *)
  val state : 'a promise -> 'a state

  (** [fulfill r x] resolves the promise [p] associated with [r] with
      value [x], meaning that [state p] will become [Fulfilled x].
      Requires: [p] is pending. *)
  val fulfill : 'a resolver -> 'a -> unit

  (** [reject r x] rejects the promise [p] associated with [r] with
      exception [x], meaning that [state p] will become [Rejected x].
      Requires: [p] is pending. *)
  val reject : 'a resolver -> exn -> unit

  (** [p >>= c] registers callback [c] with promise [p].
      When the promise is fulfilled, the callback will be run
      on the promises's contents.  If the promise is never
      fulfilled, the callback will never run. *)
  val ( >>= ) : 'a promise -> ('a -> 'b promise) -> 'b promise
end

module Promise : PROMISE = struct
  type 'a state = Pending | Fulfilled of 'a | Rejected of exn

  (** RI: the input may not be [Pending]. *)
  type 'a handler = 'a state -> unit

  (** RI: if [state <> Pending] then [handlers = []]. *)
  type 'a promise = {
    mutable state : 'a state;
    mutable handlers : 'a handler list
  }

  let enqueue
      (handler : 'a state -> unit)
      (promise : 'a promise) : unit
    =
    promise.handlers <- handler :: promise.handlers

  type 'a resolver = 'a promise

  (** [write_once p s] changes the state of [p] to be [s].  If [p] and [s]
      are both pending, that has no effect.
      Raises: [Invalid_arg] if the state of [p] is not pending. *)
  let write_once p s =
    if p.state = Pending
    then p.state <- s
    else invalid_arg "cannot write twice"

  let make () =
    let p = {state = Pending; handlers = []} in
    p, p

  let return x =
    {state = Fulfilled x; handlers = []}

  let state p = p.state

  (** Requires: [st] may not be [Pending]. *)
  let fulfill_or_reject (r : 'a resolver) (st : 'a state) =
    assert (st <> Pending);
    let handlers = r.handlers in
    r.handlers <- [];
    write_once r st;
    List.iter (fun f -> f st) handlers

  let reject r x =
    fulfill_or_reject r (Rejected x)

  let fulfill r x =
    fulfill_or_reject r (Fulfilled x)

  let handler (resolver : 'a resolver) : 'a handler
    = function
      | Pending -> failwith "handler RI violated"
      | Rejected exc -> reject resolver exc
      | Fulfilled x -> fulfill resolver x

  let handler_of_callback
      (callback : 'a -> 'b promise)
      (resolver : 'b resolver) : 'a handler
    = function
      | Pending -> failwith "handler RI violated"
      | Rejected exc -> reject resolver exc
      | Fulfilled x ->
        let promise = callback x in
        match promise.state with
        | Fulfilled y -> fulfill resolver y
        | Rejected exc -> reject resolver exc
        | Pending -> enqueue (handler resolver) promise

  let ( >>= )
      (input_promise : 'a promise)
      (callback : 'a -> 'b promise) : 'b promise
    =
    match input_promise.state with
    | Fulfilled x -> callback x
    | Rejected exc -> {state = Rejected exc; handlers = []}
    | Pending ->
      let output_promise, output_resolver = make () in
      enqueue (handler_of_callback callback output_resolver) input_promise;
      output_promise
end

let _ = 
  let my_promise, my_resolver = Promise.make () in
  let _ = Promise.(>>=) my_promise (fun n -> let _ = Printf.printf "%d\n" n in Promise.return ()) in
  Promise.fulfill my_resolver 10

(* Exercise: promise and resolve lwt *)
(* #require "lwt" "lwt.unix";; *)

let _ = 
  let my_promise, my_resolver = Lwt.wait () in 
  let _ = Lwt.bind my_promise (fun n -> Lwt_io.printf "%d\n" n) in 
  Lwt.wakeup_later my_resolver 20