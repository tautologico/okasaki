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

module UnbalancedSet(Ord : Ordered) : S with type elem := Ord.t = struct
  type tree = E | T of tree * Ord.t * tree 
  type t = tree

  let empty = E

  let rec insert v s = 
    match s with 
    | E -> T(E, v, E)
    | T(tl, x, tr) -> 
      if Ord.lt v x then T(insert v tl, x, tr)
      else if Ord.lt x v then T(tl, x, insert v tr)
      else s

  let rec member v s = 
    match s with 
    | E -> false
    | T(tl, x, tr) -> 
      if Ord.eq v x then true
      else if Ord.lt v x then member v tl
      else member v tr 
end
