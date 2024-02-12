module Unionfind :
  sig
    type t = Struct_pers.Make(Struct_pers.New_Arr).t
    val create : int -> t
    val find : t -> int -> int
    val union : t -> int -> int -> t
  end
val taille : 'a array array -> int * int
val mat_to_tab : int -> int -> int -> int
val tab_to_mat : int -> int -> int * int
val print_matrix : bool array array -> unit
val print : bool array array -> unit
val print_uf : Unionfind.t ref -> 'a array array -> unit
val deuxieme_cases_vise : int -> int -> int -> int * int
val is_in_board : 'a array array -> int -> int -> int -> bool
val is_playable : bool array array -> int -> int -> int -> bool
val init_uf : 'a array array -> Unionfind.t ref
val init_tab_case_adjacentes : int -> int -> int -> int -> (int * int) array
val tab_voisins_to_classe :
  (int * int) array -> Unionfind.t ref -> bool array array -> int array
val check_chemin : int array -> Unionfind.t ref -> bool
val init_classe_uf :
  'a array array -> (int * int * int * int) Struct_pers.New_Arr.data ref
val actualiser_classes :
  bool array array ->
  Unionfind.t ref * (int * int * int * int) Struct_pers.New_Arr.data ref
val actualiser_union_find :
  bool array array ->
  Unionfind.t ref ->
  int ->
  int ->
  int ->
  Unionfind.t ref *
  (int * int * int * int) Struct_pers.New_Arr.data ref option
val tab_post_sep :
  'a array array ->
  Unionfind.t ref ->
  (int * int * int * int) Struct_pers.New_Arr.t -> bool array array list
val play :
  bool array array ->
  Unionfind.t ref ->
  int ->
  int ->
  int ->
  Unionfind.t ref *
  (int * int * int * int) Struct_pers.New_Arr.data ref option
val perdu : bool array array -> bool
val nimber_exact_naif : bool array array -> int
val nimber_exact_moins_naif : bool array array -> int
val resultat_couple_old : bool array array -> int -> bool
val resultat_couple : bool array array -> int -> Unionfind.t ref -> bool
val nimber_non_naif : bool array array -> int

