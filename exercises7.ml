(* Exercise: mutable fields *)

type studentRecord = { name: string; mutable gpa: float }

let alice = {name="Alice"; gpa=3.7}

let () = alice.gpa <- 4.0