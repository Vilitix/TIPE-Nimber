val taille : 'a array array -> int * int
val print : bool array array -> unit
val mat_to_tab : int -> int -> int -> int
val tab_to_mat : int -> int -> int * int
val print_uf : Union_find.union_find -> 'a array array -> unit
val deuxieme_cases_vise : int -> int -> int -> int * int
val is_in_board : 'a array array-> int -> int -> int -> bool
val is_playable : bool array array -> int -> int -> int -> bool
val play : Union_find.union_find -> bool array array -> int -> int -> int -> Union_find.union_find
val init_uf : 'a array array -> Union_find.union_find
val init_tab_case_adjacentes : int -> int -> int -> int -> (int * int) array
val tab_voisins_to_classe : (int * int) array -> Union_find.union_find -> bool array array -> int array
val check_chemin : int array -> Union_find.union_find  -> bool
val actualiser_classes : bool array array -> Union_find.union_find
val actualiser_union_find :
  bool array array ->
  Union_find.union_find -> int -> int -> int -> Union_find.union_find
val print_matrix : bool array array -> unit
val init_classe_uf :bool array array -> Union_find.union_find -> unit
val tab_post_sep : bool array array -> Union_find.union_find -> (bool array array) list
val perdu : bool array array -> bool
