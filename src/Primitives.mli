
(** Primitive data types and interfaces for OCaml. *)

(** {1 Interfaces}

    Common interfaces implemented by most primitive data types. *)

(** {3 Basic} *)

(** {3 Equality and Comparison} *)

type ordering = LT | EQ | GT



module type Eq = sig
  type t

  val eq : t -> t -> bool
end


module type Ord = sig
  type t
  (** The type for ordered values. *)

  val cmp : t -> t -> ordering
  (** Returns an {!type:ordering} between two values. *)
end


(** {3 Enumerations} *)


(** Bounded represents types that have an upper and lower limit.

    Instances of this interfaces should satisfy the following law in addition
    to the [Ord] laws:

    - [min_value <= a <= max_value] *)
module type Bounded = sig
  type t
  (** The type for bounded values. *)

  val min_value : t
  (** The minimum value of the type. *)

  val max_value : t
  (** The maximum value of the type. *)

  include Ord with type t := t
end


(** Interface for sequentially ordered types.

    This interface for small ordered sum types with statically-known
    cardinality and the ability to easily compute successor and predecessor
    elements. {i e.g.} days of week. *)
module type Enum = sig
  type t

  val to_int : t -> int
  (** Converts an enumeration to an integer. *)

  val of_int : int -> t option
  (** Converts an integer to an enumeration or produces [None] if the integer
      is too large. *)

  include Ord with type t := t
end


module Enum : sig
  module type Extension = sig
    include Enum

    val succ : t -> t option
    (** [succ a] is the successor of the value [a] or [None] if [a] is the upper
        bound of the type [t]. *)

    val pred : t -> t option
    (** [prec a] is the predecessor of the value [a] or [None] if [a] is the
        lower bound of the type [t]. *)
  end
end



module type BoundedEnum = sig
  type t

  val size : int
  val to_enum : int -> t option
  val of_enum : t -> int

  (* fromEnum true *)

  include Bounded with type t := t
  include Enum    with type t := t
end



(** {3 Pretty-printing} *)

type 'a fmt = Format.formatter -> 'a -> unit


module type Show = sig
  type t

  val pp : t fmt
end


module Show : sig
  module type Extension = sig
    type t

    val show : t -> string

    val print : t -> unit

    include Show with type t := t
  end

  module Extend (M : Show) : Extension
end


module type Dump = sig
  type t

  val dump : t fmt
end



(** {3 Hashing} *)

module type Hash = sig
  type t

  val hash : t -> int
end


module type Hash1 = sig
  type 'a t

  val hash : 'a t -> int
end


module type Hash2 = sig
  type ('a, 'b) t

  val hash : ('a, 'b) t -> int
end


module Hash : sig

end


(** {3 Default} *)

module type Default = sig
  type t

  val default : t
end


module type Default1 = sig
  type 'a t

  val default : 'a t
end



(** {3 Numeric} *)

(** Basic numeric interface. *)
module type Num = sig
  type t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val (~- ) : t -> t
  val (~+ ) : t -> t
  val abs : t -> t
  val signum : t -> t
  val of_int : int -> t
end


(** {1 Data Types}

    Builtin data types in OCaml. *)

(** Unit type and operations.

    The [unit] type has a single inhabitant, called unit. It represents values
    with no computational content. *)
module Unit : sig
  type t = unit


  (** {3 Implemented instances} *)
  include Bounded with type t := t
  include Enum    with type t := t
  include Ord     with type t := t
  include Show    with type t := t
  include Hash    with type t := t
  include Default with type t := t
end


(** Boolean type and operations. *)
module Bool : sig
  type t = bool

  include Bounded with type t := t
  include Enum    with type t := t
  include Ord     with type t := t
  include Show    with type t := t
  include Hash    with type t := t
  include Default with type t := t
end


(** Integer type and operations. *)
module Int : sig
  type t = int

  val ( / ) : t -> t -> t
  (* Integer devision. *)

  val ( mod ) : t -> t -> t
  (** Integer remainder.

      The result of [a mod b] satisfies the following properties, if [b] is not zero:

      - [a = (a / b) * b + a mod b]
      - [abs(a mod b) <= abs(b) - 1].

      @raise Division_by_zero if [b = 0].

      {e Note:} the result of [a mod b] is negative only if [a < 0].

      Left-associative operator at precedence level 7/11. *)


  (** {3 Conversion } *)

  val of_float : float -> int
  (** [of_float x] is an integer representation of the float value [x]. *)

  val of_char : char -> int
  (** [of_char x] is an integer representation of the char value [x].
      See [Char.try_of_int] *)


  (** {3 Implemented instances} *)

  include Bounded with type t := t
  include Enum    with type t := t
  include Ord     with type t := t
  include Show    with type t := t
  include Hash    with type t := t
  include Default with type t := t
  include Num     with type t := t
end


(** Character type and operations.

    Character are just bytes. *)
module Char : sig
  type t = char

  (** {3 Implemented instances} *)
  include Bounded with type t := t
  include Enum    with type t := t
  include Ord     with type t := t
  include Show    with type t := t
  include Hash    with type t := t
  include Default with type t := t
end


