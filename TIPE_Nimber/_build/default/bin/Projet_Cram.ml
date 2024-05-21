module Unionfind = Struct_pers.Make (Struct_pers.New_Arr);;
(*O(1)*)
let taille table = 
  (Array.length table, Array.length table.(0));;

(*O(1)*)
let mat_to_tab i j p = 
  i*p + j;;

(*O(1)*)
let tab_to_mat k p = 
  (k/p,k mod p )



(*TEST*)
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

let print_uf uf table =
  let n,p = taille table in 
  for i = 0 to (n+1) do 
    for j = 0 to (p+1) do 
      Printf.printf "%d  " (Unionfind.find !uf (mat_to_tab i j (p+2)))
    done;
    Printf.printf "\n"
  done;
  Printf.printf "\n"

(*FIN DES FONCTIONS DE TEST*)

(*direction : (-1 haut (1 bas) (2 gauche) (-2 droite)*)
(*O(1)*)
let deuxieme_cases_vise i j direction = (* renvoie la position de la deuxieme case visée*)
  match direction with
    |(-1) ->(i-1,j)
    |1 -> (i+1,j) 
    |(-2) ->(i,j+1)
    |2 -> (i,j-1)
    |_ -> failwith "direction incorrecte"

(*O(1)*)
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

(*O(1)*)
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
    


(*O(n*p)*)
let init_uf tab =
  let n,p = taille tab in
  let uf = ref (Unionfind.create ((n+2)*(p+2))) in
(*initialisation de la classe des côtés*)
  (*réel tableau de  i = 1 à n et j de 1 à p*)
  for i = 0 to n+1 do (*car de taille n+2*) 
    uf := Unionfind.union !uf (mat_to_tab i 0 (p+2)) 0;
    uf := Unionfind.union !uf (mat_to_tab i (p+1) (p+2)) 0;
  done;
  for j = 0 to p+1 do (*car de taille p+2*) 
    uf := Unionfind.union !uf (mat_to_tab 0 j (p+2)) 0;
    uf:= Unionfind.union !uf (mat_to_tab (n+1) j (p+2)) 0;
  done;
  uf
;;


(*O(1)*)
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
      

(*O(?)*)

let tab_voisins_to_classe tab_voisin uf table =
  (*tab_res.(i) = true si la case correspondante est remplie ou un bord*)
  (*en pratique k = 10 non variable*)
  let k = Array.length tab_voisin in
  let n,p = taille table in
  let classe_0 = ref (Unionfind.find !uf 0) in 
  let tab_res = Array.make k (-1) in
  let l, j' = tab_voisin.(0) in 
  let j = j'+1 in (*coordonnée d'un point parmi les 2 cases visées*)
  for i = 0 to k-1 do 
    let x,y = tab_voisin.(i) in
    if (x = -1) || (x = n) || (y = -1) || (y = p) then 
      begin
        uf:= Unionfind.union !uf (mat_to_tab (l+1) (j+1) (p+2)) !classe_0;
        classe_0:= (Unionfind.find !uf 0);
        tab_res.(i) <- !classe_0;
        
      end
    else if (x<n && y<p) then
      begin 
        let classe = Unionfind.find !uf (mat_to_tab (x+1) (y+1) (p+2)) in
        tab_res.(i) <- classe;
        if (table.(x).(y)) then uf:= Unionfind.union !uf (mat_to_tab (l+1) (j+1) (p+2)) classe; classe_0:= (Unionfind.find !uf 0);
          (*permet d'associer à la même classe les cases remplies table à séparer s'il y a une boucle dans l'union find*)
          (*on réassocie classe_0 si elle bouge*)
      end
         else failwith "erreur dans le tableau des voisins" 
        done;
  tab_res

(*O(1) toujours le même nombre d'action tab_voisin est de taille 10*)
let check_chemin tab uf :bool =  
  (*trouve s'il y a un chemin entre deux points i j de i à j et de j à i ayant au moins une case true  *)
  (*c'est la conditon pour lancer le union find (signifie que le plateau est séparable)*)
  (*en pratique k = 10 constant*)
  let k = Array.length tab in 
  let res = ref false in
  let classe_0 = Unionfind.find !uf 0 in
  for i = 0 to k-1 do 
    for j = 0 to k-1 do 
      if (((i-j)>1) && (tab.(i) != tab.(j)) && (tab.(i) != classe_0 ) && (tab.(j) != classe_0 )) then
        (*condition: chemin entre deux points non adjacent non bords non cases pleines*)
        begin
          for m = j+1 to i-1 do 
            let l = ref ((i + 1) mod k) in (*chemin extérieur*)
            while (!l != j) do 

              if tab.(m) = tab.(!l) then 
                  res := true;
                 
                  l:= (!l+1) mod k;
            done;
          done;
        end
      done;
    done;
    (!res)

(*O(n*p)*)
let init_classe_uf table = 
  let n,p = taille table in
    (*indice qui s'explique par les bords qu'on évite par la suite*)
    let f k = 
    let i,j = tab_to_mat k (p+2) in
    
    if ((j != 0) && (i!= 0) && (j!= p+1) && (i!= n+1)) then
    i,i,j,j
    else 0,0,0,0
  in
  Struct_pers.New_Arr.init ((n+2)*(p+2)) f
  

(*O(n*p*?) ne s'execute que s'il y a séparation*)
let actualiser_classes table =
  let n, p = taille table in
  let uf = init_uf table in
  let tab_c = ref (init_classe_uf table) in
  let classe_0 = ref (Unionfind.find !uf 0) in
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
              let classe_to_add = Unionfind.find !uf (mat_to_tab (i + 1) (j + 2) (p + 2)) in
              let xmin_i,xmax_i,ymin_i,ymax_i = Struct_pers.New_Arr.get !tab_c (classe_to_add) in
              uf := Unionfind.union !uf (mat_to_tab (i + 1) (j + 1) (p + 2)) (mat_to_tab (i + 1) (j + 2) (p + 2));
              let classe_finale = Unionfind.find !uf (mat_to_tab (i + 1) (j + 1) (p + 2)) in 
              let xmin,xmax,ymin,ymax = Struct_pers.New_Arr.get !tab_c (classe_finale) in
              tab_c := Struct_pers.New_Arr.set !tab_c (classe_finale) ((min xmin_i xmin),(max xmax xmax_i),(min ymin ymin_i),(max (max ymax (j+2)) ymax_i)) (*+2 car on étudie déjà j+1*)

          | _, y when y = p - 1 ->
            if not(table.(i + 1).(j)) then
              let classe_to_add = Unionfind.find !uf (mat_to_tab (i + 2) (j + 1) (p + 2)) in
              let xmin_i,xmax_i,ymin_i,ymax_i = Struct_pers.New_Arr.get !tab_c (classe_to_add) in
              uf := Unionfind.union !uf (mat_to_tab (i + 1) (j + 1) (p + 2)) (mat_to_tab (i + 2) (j + 1) (p + 2));
              let classe_finale = Unionfind.find !uf (mat_to_tab (i + 1) (j + 1) (p + 2)) in 
              let xmin,xmax,ymin,ymax = Struct_pers.New_Arr.get !tab_c (classe_finale) in
              tab_c := Struct_pers.New_Arr.set !tab_c (classe_finale) ((min xmin xmin_i),(max (max xmax (i+2)) xmax_i),(min ymin ymin_i),(max ymax ymax_i)) (*+2 car on étudie déjà i+1*)

          | _, _ ->
              let classe_finale = Unionfind.find !uf (mat_to_tab (i + 1) (j + 1) (p + 2)) in 
              
              let xmin,xmax,ymin,ymax = Struct_pers.New_Arr.get !tab_c (classe_finale) in
              if not(table.(i + 1).(j)) then (
                
                let classe_to_add1 = Unionfind.find !uf (mat_to_tab (i + 2) (j + 1) (p + 2)) in
              let xmin_i,xmax_i,ymin_i,ymax_i = Struct_pers.New_Arr.get !tab_c (classe_to_add1) in
                uf:= Unionfind.union !uf (mat_to_tab (i + 1) (j + 1) (p + 2)) (mat_to_tab (i + 2) (j + 1) (p + 2));
                tab_c := Struct_pers.New_Arr.set !tab_c (classe_finale)  ((min xmin xmin_i),(max (max xmax (i+2)) xmax_i),(min ymin ymin_i),(max ymax ymax_i)
                ));

              if not(table.(i).(j + 1)) then (

                let classe_to_add2 = Unionfind.find !uf (mat_to_tab (i + 1) (j + 2) (p + 2)) in
              let xmin_i,xmax_i,ymin_i,ymax_i = Struct_pers.New_Arr.get !tab_c (classe_to_add2) in
                uf:= Unionfind.union !uf (mat_to_tab (i + 1) (j + 1) (p + 2)) (mat_to_tab (i + 1) (j + 2) (p + 2));
                tab_c := Struct_pers.New_Arr.set !tab_c (classe_finale) ((min xmin_i xmin),(max xmax xmax_i),(min ymin ymin_i),(max (max ymax (j+2)) ymax_i)
                ));
              
        end
      else uf := Unionfind.union !uf (mat_to_tab (i + 1) (j + 1) (p + 2)) !classe_0 ; classe_0:= Unionfind.find !uf 0
    done;
  done;
  uf,!tab_c

    

(*O(n*p*?) si séparation sinon O(?)*)
let actualiser_union_find table uf i j direction  =
  let _,p = taille table in 
  if (is_in_board table i j direction) then 
    let k,l = deuxieme_cases_vise i j direction in
    uf:= Unionfind.union !uf (mat_to_tab (k+1) (l+1) (p+2)) (mat_to_tab (i+1) (j+1) (p+2)) ;
    let tab_voisins = init_tab_case_adjacentes i j k l in
    let tab_voisins_classe = tab_voisins_to_classe tab_voisins uf table in
    if check_chemin tab_voisins_classe uf then
      (print_uf uf table;
      let uf, new_tab_c = actualiser_classes table in 
      uf, Some new_tab_c)
    else uf, None
  else failwith "coup incorrect"

(*O(n*p*alpha(n))*)
let tab_post_sep table uf tab_c = 
  let n,p = taille table in
  let liste_res = ref [] in
  (*liste de taille au max 4 car un coup peut séparer le tableau en 4 parties au maximum*)
  let classe_0 = Unionfind.find !uf 0 in
  for i = 0 to (((n+2)*(p+2))-1) do 
    let classe_en_cours = Unionfind.find !uf i in
    if ((i = (classe_en_cours)) && (classe_en_cours != classe_0)  )  then (
        
        let xmin,xmax,ymin,ymax = Struct_pers.New_Arr.get tab_c i in
        let new_tab = Array.make_matrix (xmax-xmin+1) (ymax-ymin+1) true in
        liste_res:= (new_tab,i,xmin,xmax,ymin,ymax) :: !liste_res
             
    );

    let k,l = tab_to_mat i (p+2) in
    if ((k!= 0) && (l!=0) && (k!=n+1) && (l!= p+1) ) then 
      begin
      (*O(alpha(n)) car liste de taille <= 4*)
    let rec parcourir_liste liste = 
      match liste with 
      |[] -> () (*pas d'erreur si grosse partie recouverte de jetons ils n'ont pas de tableau *)
      |(tab,j,xmin,xmax,ymin,ymax)::q -> 
        begin
          if (k>=xmin && k<=xmax && l>=ymin && l<=ymax) then 
            tab.(k-xmin).(l-ymin) <- (Unionfind.find !uf (mat_to_tab (k) (l) (p+2))) != j
            
          else parcourir_liste q
        end
      in
      parcourir_liste !liste_res
    end
  done;
  let f l = let (tab,_,_,_,_,_) = l in tab in
  List.map f !liste_res




let play table uf i j direction = 
  if (is_playable table i j direction) then 
    begin
    table.(i).(j) <- true;
    let k,l = deuxieme_cases_vise i j direction in 
    table.(k).(l) <- true;
    actualiser_union_find table uf i j direction 
    end
  else failwith "coup incorrect"
;; 
    
let perdu table =
  let n,p = taille table in
  let res = ref true in
  let tab_direction = [|-1;1;-2;2|] in
  for k = 0 to 3 do 
    for i = 0 to (n-1) do 
      for j = 0 to (p-1) do 
        if (is_playable table i j tab_direction.(k)) then 
          begin
            res := false;
          end
      done;
    done;
  done;
  !res

  (*
let iteration parameter n p:((int*int*int) list) =
  let l = ref []
  match parameter with 
  |0-> 
    for j = n-1 to 0 do 
      for k = 0 to 1 do 
        for i = p-1 downto 0 do 
          l:= ::!l

*)
(*note pour plus tard lorsque l'on separe on refait bien tout l'union find pcq sinon lorsqu'on joue l'union de la classe de la nouvelle case 
   qui est dans la même classe que toutes les autres met tout dans la même classe
   besoin d'actualiser les coordonnée dans ce cas pour séparer*)

  (*ajouter avec unir autour des classes d equi et si c est la classe d equi des truck posés  alors actualise *)


(*let reso_naive table = 
  let n,p = taille table in
  let uf = init_uf table in
  let tab_direction = [|-1;1;-2;2|] in
  (*joueur 0 est premier joueur*)
  let rec reso_naive_rec table uf i j direction joueur = 
    if (perdu table) then joueur
    else 
      for i = 0 to 3 do 
        if (is_playable table i j tab_direction.(i)) then 
            reso_naive_rec table (play uf table i j tab_direction.(i)) i j tab_direction.(i) (1-joueur)
      done;
*)


let iter table (i,j,k) =
  let n, p = taille table in
  match (i, j, k) with
  | 0, y, (- 2) when y = p-2-> (n - 1), 0, -1 (* fin horizontal *)
  | 1, y, -1 when y = p-1-> -1, -1, -1 (* fin vertical, fin tout court *)
  | x, y, -2 when y = p-2-> (x - 1), 0, -2 (* fin d'une ligne en horizontal *)
  | x, y, -1 when y = (p-1)-> (x - 1), 0, -1
  | x, y, -2 -> x, (y + 1), -2
  | x, y, -1 -> x, (y + 1), -1
  |_ -> failwith "probleme d'itération"
;;
let nimber_exact_naif table = 
  let n,p = taille table in
  let tab_direction = [|-1;1;-2;2|] in
  let rec calcul_nim table = 
    (*cas de base déjà dedans le mex  d'un ensemble vide est 0*)
    let set = ref Nim_func.SS.empty in
    for i = 0 to (n-1) do 
      for j = 0 to (p-1) do 
        for k = 0 to 3 do 
          if (is_playable table i j tab_direction.(k)) then 
            begin
              table.(i).(j) <- true;
              let l,m = deuxieme_cases_vise i j tab_direction.(k) in
              table.(l).(m) <- true;
              set := Nim_func.SS.add ((calcul_nim table)) !set;
              table.(i).(j) <- false;
              table.(l).(m) <- false;
            end
        done;
      done;
    done;
    Nim_func.mex !set
    in
  calcul_nim table




let nimber_exact_moins_naif table = 
  let n,p = taille table in
  let tab_direction = [|-1;1;-2;2|] in
  let uf = init_uf table in
  let rec calcul_nim table uf = 
    (*cas de base déjà dedans le mex  d'un ensemble vide est 0*)
    let set = ref Nim_func.SS.empty in
    for i = 0 to (n-1) do 
      for j = 0 to (p-1) do 
        for k = 0 to 3 do 
          if (is_playable table i j tab_direction.(k)) then 
            begin
              table.(i).(j) <- true;
              let l,m = deuxieme_cases_vise i j tab_direction.(k) in
              table.(l).(m) <- true;
              let new_uf, new_tab_c = actualiser_union_find table uf i j tab_direction.(k) in
              begin
                match new_tab_c with
                |None -> set := Nim_func.SS.add ((calcul_nim table new_uf)) !set;
                |Some tab_c -> 
                  let tab_post_sep = tab_post_sep table new_uf tab_c in
                  let f tab = set := Nim_func.SS.add ((calcul_nim tab new_uf)) !set in
                  List.iter f tab_post_sep
              end;
              table.(i).(j) <- false;
              table.(l).(m) <- false;
            end
        done;
      done;
    done;
    Nim_func.mex !set
    in
  calcul_nim table uf




