module Unionfind :
  sig
    type t = Struct_pers.Make(Struct_pers.New_Arr).t
    val create : int -> t
    val find : t -> int -> int
    val union : t -> int -> int -> t
  end

val resultat_couple : bool array array -> int -> Unionfind.t ref -> bool
val nimber_non_naif : bool array array -> int 