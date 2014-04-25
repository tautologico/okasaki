
module type Ordered = sig
  type t

  val eq  : t -> t -> bool
  val lt  : t -> t -> bool
  val leq : t -> t -> bool
end

module type S = sig
  type elem
  type t

  val empty : t
  val insert : elem -> t -> t
  val member : elem -> t -> bool
end

module UnbalancedSet (Ord : Ordered) : S with type elem := Ord.t
