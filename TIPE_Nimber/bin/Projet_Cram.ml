
let taille table = 
  (Array.length table, Array.length table.(0));;

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



let mat_to_tab i j p = 
  i*p + j;;

let tab_to_mat k p = 
  (k/p,k mod p )

(*direction : (-1 haut (1 bas) (2 gauche) (-2 droite)*)
let deuxieme_cases_vise i j direction = (* renvoie la position de la deuxieme case visée*)
  match direction with
    |(-1) ->(i-1,j)
    |1 -> (i+1,j) 
    |(-2) ->(i,j+1)
    |2 -> (i,j-1)
    |_ -> failwith "direction incorrecte"


let is_in_board tab i j direction = (* pour vérifier que rien ne déborde*)
  let res = ref false in
  let n,p = taille tab in
  if (i<n && j<p) then 
    match direction with 
    |(-1) -> if (i> 0) then res:= true
    |1 -> if ((i+1)< n) then res:= true
    |(-2) -> if ((j+1)< p) then res:= true
    |2 -> if (j>0) then res:= true
    |_ -> ();
  else ();
  !res


let is_playable tab i j direction = 
  let res = ref false in
  if ((is_in_board tab i j direction) && (not(tab.(i).(j)))) then 
    match direction with 
    |(-1) -> if not(tab.(i-1).(j)) then res:= true
    |1 -> if not(tab.(i+1).(j)) then res:= true
    |(-2) -> if not(tab.(i).(j+1)) then res:= true
    |2 -> if not(tab.(i).(j-1)) then res:= true
    |_ -> ();
  else ();
  !res
    
  (*ajouter avec unir autour des classes d equi et si c est la classe d equi des truck posés  alors actualise *)

(* peudo code algo 
let algorithm1 tab n = 
  if 2 classe d'équi au moins faire algo 2
  else 
    for i = 0 to n do 
        for j = 0 to tous les coups possible
      if (algorithm1 tab(lecoup) i) then true
      mex 
    
let play_uf i j  table uf  = (* on a eu Union_find.find x = Union_find.find x après avoir jouer un pion *)
  



    let split table tab_uf = 
      let n,p = taille table in
      let classe_case_pleine = ref 0 in 
      let classe_1 = ref 0 in
      for i = 0 to (n-1) do 
        for j = 0 to (p-1) do 
          if table.()



let calcul_couple table n = 
  let uf = actualiser_union_find table in
  let l = split table (Union.find get_tab uf) in 
  match l with

let calcul_somme_couple table uf l

let nimber table cap = 
  let i = ref 0 
  while (not(calcul_couple table n) && (i<= cap) ) do 
    i := i+1
  done;
  if i = (cap) then failwith "le nimber est supérieur au max précisé"
  else i
;;
*)

let init_uf tab = 
 let n,p = taille tab in
  let uf = Union_find.init ((n+2)*(p+2)) (*taille n*p + la classe d'équivalence des bords *) in 
  (*initialisation de la classe des côtés*)
  (*réel tableau de  i = 1 à n et j de 1 à p*)
  for i = 0 to n+1 do (*car de taille n+2*) 
    Union_find.union uf (mat_to_tab i 0 (p+2)) 0;
    Union_find.union uf (mat_to_tab i (p+1) (p+2)) 0;
  done;
  for j = 0 to p+1 do (*car de taille p+2*) 
    Union_find.union uf (mat_to_tab 0 j (p+2)) 0;
    Union_find.union uf (mat_to_tab (n+1) j (p+2)) 0;
  done;
  uf
;;

let print_uf uf table =
  let n,p = taille table in 
  for i = 0 to (n+1) do 
    for j = 0 to (p+1) do 
      Printf.printf "%d  " (Union_find.find uf (mat_to_tab i j (p+2)))
    done;
    Printf.printf "\n"
  done;
  Printf.printf "\n"


let init_tab_case_adjacentes i j k l = (*renvoie les 10 cases adjacentes aux points i j et k l*)
  let tab_voisins = Array.make 10 (-2,-2) in (*-2 valeur impossible car le coup est jouable*)
    tab_voisins.(0) <- (min i k),((min j l)-1);
    tab_voisins.(1) <- ((min i k)-1),((min j l)-1);
    tab_voisins.(2) <- ((min i k)-1),(min j l);
    tab_voisins.(3) <- ((min i k)-1),((min j l)+1);

    if i = k then
      begin
        tab_voisins.(4) <- (i-1),((max j l)+1);
        tab_voisins.(5) <- (i),((max j l)+1);
        tab_voisins.(6) <- (i+1),((max j l)+1);
        tab_voisins.(7) <- (i+1),((max j l));
        tab_voisins.(8) <- (i+1),((max j l)-1);
        tab_voisins.(9) <- (i+1),((max j l)-2);
      end
    else 
        begin
        if j = l then 
      begin
        tab_voisins.(4) <- ((min i k)),((max j l)+1);
        tab_voisins.(5) <- ((min i k)+1),((max j l)+1);
        tab_voisins.(6) <- ((min i k)+2),((max j l)+1);
        tab_voisins.(7) <- ((min i k)+2),((max j l));
        tab_voisins.(8) <- ((min i k)+2),((max j l)-1);
        tab_voisins.(9) <- ((min i k)+1),((max j l)-1);
      end
    else failwith "impossible le coup est jouable donc les cases adjacentes"
      end;
      tab_voisins
      

let tab_voisins_to_classe tab_voisin uf table =
  (*tab_res.(i) = true si la case correspondante est remplie ou un bord*)
  let k = Array.length tab_voisin in
  let n,p = taille table in
  let classe_0 = ref (Union_find.find uf 0) in 
  let tab_res = Array.make k (-1) in
  let l, j' = tab_voisin.(0) in 
  let j = j'+1 in (*coordonnée d'un point parmi les 2 cases visées*)
  for i = 0 to k-1 do 
    let x,y = tab_voisin.(i) in
    if (x = -1) || (x = n) || (y = -1) || (y = p) then 
      begin
        Union_find.union uf (mat_to_tab (l+1) (j+1) (p+2)) !classe_0;
        classe_0:= (Union_find.find uf 0);
        tab_res.(i) <- !classe_0;
        
      end
    else if (x<n && y<p) then
      begin 
        let classe = Union_find.find uf (mat_to_tab (x+1) (y+1) (p+2)) in
        tab_res.(i) <- classe;
        if (table.(x).(y)) then Union_find.union uf (mat_to_tab (l+1) (j+1) (p+2)) classe; classe_0:= (Union_find.find uf 0);
          (*permet d'associer à la même classe les cases remplies table à séparer s'il y a une boucle dans l'union find*)
      end
         else failwith "erreur dans le tableau des voisins" 
        done;
  tab_res


let check_chemin tab uf :bool =  
  (*trouve s'il y a un chemin entre deux points i j de i à j et de j à i ayant au moins une case true  *)
  (*c'est la conditon pour lancer le union find (signifie que le plateau est séparable)*)
  let n = Array.length tab in 
  let res = ref false in
  let classe_0 = Union_find.find uf 0 in
  for i = 0 to n-1 do 
    for j = 0 to n-1 do 
      if (((i-j)>1) && (tab.(i) != tab.(j)) && (tab.(i) != classe_0 ) && (tab.(j) != classe_0 )) then
        (*condition: chemin entre deux points non adjacent non bords non cases pleines*)
        begin
          for k = j+1 to i-1 do 
            let l = ref ((i + 1) mod n) in (*chemin extérieur*)
            while (!l != j) do 

              if tab.(k) = tab.(!l) then 
                  (res := true;Printf.printf "i =%d, j = %d, k = %d, l = %d\n" i j k !l);
                 
                  l:= (!l+1) mod n;
            done;
          done;
        end
      done;
    done;
    (!res)
;;


  let actualiser_classes table =
    let n, p = taille table in
    let uf = init_uf table in
    let classe_0 = ref (Union_find.find uf 0) in
    (* Les indices i et j s'expliquent par la présence de bords *)
    (* Donc, les indices sont décalés de 1 dans table *)
    for i = 0 to n - 1 do
      for j = 0 to p - 1 do
        if not table.(i).(j) then
          begin
            match i, j with
            | x, y when x = n - 1 && y = p - 1 -> ()
            | x, _ when x = n - 1 ->
              if not(table.(i).(j + 1)) then
                Union_find.union uf (mat_to_tab (i + 1) (j + 1) (p + 2)) (mat_to_tab (i + 1) (j + 2) (p + 2))
            | _, y when y = p - 1 ->
              if not(table.(i + 1).(j)) then
                Union_find.union uf (mat_to_tab (i + 1) (j + 1) (p + 2)) (mat_to_tab (i + 2) (j + 1) (p + 2))
            | _, _ ->
              if not(table.(i + 1).(j)) then Union_find.union uf (mat_to_tab (i + 1) (j + 1) (p + 2)) (mat_to_tab (i + 2) (j + 1) (p + 2));
              if not(table.(i).(j + 1)) then Union_find.union uf (mat_to_tab (i + 1) (j + 1) (p + 2)) (mat_to_tab (i + 1) (j + 2) (p + 2))
          end
        else Union_find.union uf (mat_to_tab (i + 1) (j + 1) (p + 2)) !classe_0 ; classe_0:= Union_find.find uf 0
      done;
    done;
    uf
  

let actualiser_union_find table uf i j direction  =
  let _,p = taille table in 
  if (is_in_board table i j direction) then 
    let k,l = deuxieme_cases_vise i j direction in
    Union_find.union uf (mat_to_tab (k+1) (l+1) (p+2)) (mat_to_tab (i+1) (j+1) (p+2)) ;
    let tab_voisins = init_tab_case_adjacentes i j k l in
    let tab_voisins_classe = tab_voisins_to_classe tab_voisins uf table in
    if check_chemin tab_voisins_classe uf then
      (Printf.printf "le plateau est séparable\n";
      print_uf uf table;
      actualiser_classes table)
    else uf
  else failwith "coup incorrect"

let play uf table i j direction = 
  if (is_playable table i j direction) then 
    begin
    table.(i).(j) <- true;
    let k,l = deuxieme_cases_vise i j direction in 
    table.(k).(l) <- true;
    actualiser_union_find table uf i j direction 
    end
  else failwith "coup incorrect"
;; 
    
     
let print_matrix mat = 
  let n,p = taille mat in 
  for i = 0 to (n-1) do 
    for j = 0 to (p -1) do 
      if mat.(i).(j) then
      Printf.printf "1  " 
      else 
      Printf.printf "0  "
    done;
    Printf.printf "\n"
  done;
  Printf.printf "\n"

(*note pour plus tard lorsque l'on separe on refait bien tout l'union find pcq sinon lorsqu'on joue l'union de la classe de la nouvelle case 
   qui est dans la même classe que toutes les autres met tout dans la même classe*)