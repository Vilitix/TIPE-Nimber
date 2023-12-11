
let taille table = 
  (Array.length table.(0),Array.length table);;

let print (table)  = 
  let n,p = taille table in 
  for i = 0 to (n-1) do 
    for j = 0 to (p -1) do 
      if (table.(i).(j)) then 
        Printf.printf "0 " 
      else Printf.printf "  "
    done;
    Printf.printf "\n"
  done;
;;



let is_empty table i j = 
  table.(i).(j);;

let mat_to_tab i j p = 
  i*p + j;;

let tab_to_mat k p = 
  (k/p,k mod p )


let actualiser_union_find tab = 
  let n,p = taille tab in
  let uf = Union_find.init (n*p) (*taille n*m*) in 
  let case_full = ref (-1) in 

  (*permet de remplir les cases occupées dans une même classe d'équivalence*)
  for i = 0 to (n-1) do 
    for j = 0 to (p-1) do 
      if (tab.(i).(j)) then 
        begin
          if (!case_full = (-1)) then case_full := (mat_to_tab i j p)
          else Union_find.union uf (mat_to_tab i j p) !case_full
        end
      done;
    done;

  (*pour mettre le reste des cases dans leurs classes*)

  for i = 0 to (n-1) do 
    for j = 0 to (p-1) do 
      if not(tab.(i).(j)) then 
      match i,j with 
      |x,y when ((x = (n-1)) && (y = (p-1))) -> ()
      |x,_ when (x = n-1) -> if (tab.(i).(j+1)) then Union_find.union uf (mat_to_tab i j p) (mat_to_tab i (j+1) p)
      |_,y when (y = (p-1)) ->  if (tab.(i+1).(j)) then Union_find.union uf (mat_to_tab i j p) (mat_to_tab (i+1) j p)
      |_,_ -> 
        begin 
          if (tab.(i+1).(j)) then Union_find.union uf (mat_to_tab i j p) (mat_to_tab i (j+1) p);
          if (tab.(i).(j+1)) then Union_find.union uf (mat_to_tab i j p) (mat_to_tab (i+1) j p)
        end
      done;
    done;
    uf
  

let print_uf uf table = 
  let n,p = taille table in 
  for i = 0 to (n-1) do 
    for j = 0 to (p -1) do 
      Printf.printf "%d  " (Union_find.find uf (mat_to_tab i j p))
    done;
    Printf.printf "\n"
  done;
;;

        
(*direction : (-1 gauche) (1 droite) (2 haut) (-2 bas)*)
let is_playable tab i j direction =
  let res = ref false in
  let n,p = taille tab in
  if (i<n && j<p && (not(tab.(i).(j)))) then 
    match direction with 
    |(-1) -> if ((i> 0) && (not(tab.(i-1).(j)))) then res:= true
    |1 -> if (((i+1)< n) && (not(tab.(i+1).(j)))) then res:= true
    |(-2) -> if (((j+1)< p) && (not(tab.(i).(j+1)))) then res:= true
    |2 -> if ((j>0) && (not(tab.(i).(j-1)))) then res:= true
    |_ -> ();
  else ();
  !res


(*direction : (-1 gauche) (1 droite) (2 haut) (-2 bas)*)
let play tab i j direction = 
  if (is_playable tab i j direction) then 
    tab.(i).(j) <- true;
    match direction with
    |(-1) -> tab.(i-1).(j) <- true
    |1 -> tab.(i+1).(j) <- true
    |(-2) -> tab.(i).(j+1) <- true
    |2 -> tab.(i).(j-1) <- true 
    |_ -> ()
;; 
    
  (*ajouter avec unir autour des classes d equi et si c est la classe d equi des truck posés  alors actualise *)

(* peudo code algo 
let algorithm1 tab n = 
  if 2 classe d'équi au moins faire algo 2
  else 
    for i = 0 to n do 
        for j = 0 to tous les coups possible
      if (algorithm1 tab(lecoup) i) then true
      mex 
    *)

