
(* Interfaces *)


module Ordering = struct
  type t = ordering

  let inspect formatter t =
    match t with
    | `Less    -> Format.fprintf formatter "Less"
    | `Equal   -> Format.fprintf formatter "Equal"
    | `Greater -> Format.fprintf formatter "Greater"

  let display formatter t =
    match t with
    | `Less    -> Format.fprintf formatter "<"
    | `Equal   -> Format.fprintf formatter "="
    | `Greater -> Format.fprintf formatter ">"

  let equal t1 t2 =
    match t1, t2 with
    | `Less, `Less
    | `Equal, `Equal
    | `Greater, `Greater -> true
    | _ -> false

  let compare t1 t2 =
    match t1, t2 with
    | `Less, `Less -> `Equal
    | `Equal, `Equal -> `Equal
    | `Greater, `Greater -> `Equal
    | `Less, _  -> `Less
    | `Equal, `Less -> `Greater
    | `Equal, `Greater -> `Less
    | `Greater, _  -> `Greater

  let to_int t =
    match t with
    | `Less -> -1
    | `Equal -> 0
    | `Greater -> 1

  let of_int i =
    if i < 0 then `Less    else
    if i > 0 then `Greater else
    `Equal

  let invert t =
    match t with
    | `Less -> `Greater
    | `Equal -> `Equal
    | `Greater -> `Less
end



module type Bounded = sig
  type t

  val min_value : t
  val max_value : t
end

type ordering = LT | EQ | GT

module type Eq = sig
  type t

  val eq : t -> t -> bool
end


module type Ord = sig
  type t

  val cmp : t -> t -> ordering
end

module type Enum = sig
  type t
  val to_int : t -> int
  val of_int : int -> t option
  include Ord with type t := t
end


module Enum = struct
  module Extend(Base : Enum) = struct
  (* Enum *)

  let succ a =
    if a < max_value then
      Some Pervasives.(char_of_int (int_of_char a + 1))
    else
      None
end


module Unit = struct
  type t = unit

  let min_value = ()
  let max_value = ()
end


module Int = struct
  type t = unit

  let min_value = min_int
  let max_value = max_int

 let ( / ) = Pervasives.( / )

  let ( mod ) = Pervasives.( mod)

  let to_string self =
    Pervasives.string_of_int self

  let of_char = Pervasives.int_of_char

  let of_float = P.int_of_float
end


module Char = struct
  type t = char

  (* Ord *)
  let std_cmp : char -> char -> int = Pervasives.compare
  let cmp a b =
    Ordering.of_int (std_cmp a b)


  (* Bounded *)
  let min_value = char_of_int 0x00
  let max_value = char_of_int 0xFF

  (* Enum *)
  include Enum.Extend(struct
      let to_int = Pervasives.int_of_char

      let of_int a =
        try Some (Pervasives.int_of_char)
        with Invalid_argument _ -> None

      let cmp = cmp
    end)

end

