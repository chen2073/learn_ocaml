module type Ring = sig
  type t

  val zero : t

  val one : t

  val ( + ) : t -> t -> t

  val ( ~- ) : t -> t

  val ( * ) : t -> t -> t

  val to_string : t -> string

  val of_int : int -> t
end

module type Field = sig
  include Ring
  val ( / ) : t -> t -> t
end

module IntRing : Ring with type t = int = struct
  type t = int

  let zero = 0

  let one = 1

  let ( + ) = ( + )

  let ( ~- ) = ( ~- )

  let ( * ) = ( * )

  let to_string = string_of_int

  let of_int n = n
end

module IntField : Field with type t = int = struct
  include IntRing
  let ( / ) = ( / )

end 

module FloatRing : Ring with type t = float = struct
  type t = float

  let zero = 0.

  let one = 1.

  let ( + ) = ( +. )

  let ( ~- ) = ( ~-. )

  let ( * ) = ( *. )

  let to_string = string_of_float

  let of_int n = float_of_int n
end

module FloatField : Field with type t = float = struct
  include FloatRing

  let ( / ) = ( /. )

end

module IntRational : Field = struct
  type t = int * int

  let zero = (0, 1)

  let one = (1, 1)

  let ( + ) (a, b) (c, d) = ((a * d) + (c * b), b * d)

  let ( ~- ) (a, b) = (-a, b)

  let ( / ) (a, b) (c, d) = (a * d, b * c)

  let ( * ) (a, b) (c, d) = (a * c, b * d)

  let to_string (a, b) = string_of_int a ^ "/" ^ string_of_int b

  let of_int n = (n, 1)
end

module FloatRational : Field = struct
  type t = float * float

  let zero = (0., 1.)

  let one = (1., 1.)

  let ( + ) (a, b) (c, d) = ((a *. d) +. (c *. b), b *. d)

  let ( ~- ) (a, b) = (-.a, b)

  let ( / ) (a, b) (c, d) = (a *. d, b *. c)

  let ( * ) (a, b) (c, d) = (a *. c, b *. d)

  let to_string (a, b) = string_of_float a ^ "/" ^ string_of_float b

  let of_int n = (float_of_int n, 1.)
end
