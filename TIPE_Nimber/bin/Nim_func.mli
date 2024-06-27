module SS : Set.S with type elt = int (*Implémenté avec des AVL, SS.add: O(log n) SS.mem: O(log n)*)
val mex : SS.t -> int 
val decomposition : int -> int array (*Décomposition en puissance de deux dans un tableau*)
val add : int -> int -> int (*XOR sur chaque bit puis renvoi le résultat, lxor fait aussi la même chose en OCaml*)
val multi : int -> int -> int
