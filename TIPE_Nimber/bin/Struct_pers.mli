module New_Arr :
  sig
    type 'a t = 'a data ref
    and 'a data = Arr of 'a array | N of int * 'a * 'a t
    val init : int -> (int -> 'a) -> 'a data ref
    val reroot : 'a t -> unit
    val get : 'a t -> int -> 'a
    val set : 'a data ref -> int -> 'a -> 'a data ref
  end
module type PersistentArray =
  sig
    type 'a t
    val init : int -> (int -> 'a) -> 'a t
    val get : 'a t -> int -> 'a
    val set : 'a t -> int -> 'a -> 'a t
  end
module type PersistentUnionFind =
  sig
    type t
    val create : int -> t
    val find : t -> int -> int
    val union : t -> int -> int -> t
  end
module Make : functor (_ : PersistentArray) -> PersistentUnionFind
