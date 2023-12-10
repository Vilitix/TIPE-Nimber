let decomp_212 board i = (*decoupe le jeu en somme de jeu pour le patterne ...212... = ...2+2... (somme de jeux) *)
  let n = Array.length board in 
  let left_board = Array.sub board 0 i in
  let right_board = Array.sub board (i+1) (n-(i+1)) in
  left_board,right_board
   ;;

let search_pattern board:int array list = (*renvoi une liste de tous les jeu *)
  let n = Array.length board in 
  let rec aux board l i j:(int array list) = (*algorithme de recherche dans un texte pas intéressant pour mot de taille 3 dans un alphabet de taille 3 *)
    if i < (n-3) then 
      match  board.(i),board.(i+1),board.(i+2) with
      |2,1,2-> 
        begin 
          let l1,_ = decomp_212 (Array.sub board j (n-j)) ((i+1)-j) in 
          aux board (l1::l) (i+1) (i+2)
        end
      |_,_,_->  aux board l (i+1) j
    else (Array.sub board j (n-j))::l
  in
  aux board [] 0 0;;


(*
let rec nim_naif board = (* colonnes supposées non vides*)
  let set = ref Nim_func.SS.empty in
  let n = Array.length board in
  for i = 0 to (n - 1) do
    if (i = 0) then
      begin 
      set := Nim_func.SS.add (nim_naif (Array.sub board 2 (n-2))) !set;
      if (board.(0) != 1) then set := Nim_func.SS.add (nim_naif (Array.sub board 1 (n-1))) !set
      end
    else if (i = (n - 1)) then
      begin
            if board.(i) = 2 then
              set := Nim_func.SS.add (nim_naif (Array.sub board 0 (n - 1))) !set;
      end
    else
      begin
        set := Nim_func.SS.add (Nim_func.add (nim_naif (Array.sub board 0 i)) (nim_naif (Array.sub board (i + 2) (n - i - 2)))) !set;
        if (board.(i) != 1)
        then set := Nim_func.SS.add (Nim_func.add (nim_naif (Array.sub board 0 i)) (nim_naif (Array.sub board (i + 1) (n - i - 1)))) !set
      end
    done;
    (Nim_func.mex !set)
  ;;
 *)
let rec nim_naif board = (* colonnes supposées non vides*)
  let set = ref Nim_func.SS.empty in
  let n = Array.length board in
  for i = 0 to (n - 1) do
    if i = 0 then
      begin 
        if ((n = 1) && (board.(i) = 2)) then set:= Nim_func.SS.singleton 0;
        if (n=2) then set := Nim_func.SS.add 0 !set;
        if (n >= 3) then
          set := Nim_func.SS.add (nim_naif (Array.sub board 2 (n - 2))) !set;
        if (board.(0) != 1) && (n >= 2) then
          set := Nim_func.SS.add (nim_naif (Array.sub board 1 (n - 1))) !set
      end
    else if i = (n - 1) then
      begin
        if (n >= 2) && (board.(i) = 2) then
          set := Nim_func.SS.add (nim_naif (Array.sub board 0 (n - 1))) !set
      end
    else
      begin
        if (i = n-2) then set := Nim_func.SS.add (nim_naif (Array.sub board 0 (n - 2))) !set;
        if (i + 2 < n) then
          set := Nim_func.SS.add (Nim_func.add (nim_naif (Array.sub board 0 i)) (nim_naif (Array.sub board (i + 2) (n - i - 2)))) !set;
        if (board.(i) != 1) && (i + 1 < n) then
          set := Nim_func.SS.add (Nim_func.add (nim_naif (Array.sub board 0 i)) (nim_naif (Array.sub board (i + 1) (n - i - 1)))) !set
      end
  done;
  (Nim_func.mex !set)
;;

let is_kayles board =
  let res = ref true in
  for i = 0 to ((Array.length board )-1) do 
    if ((board.(i) = 1)||(board.(i) = 0)) then res:= false
  done;
  !res;;




    
let nim board =  
  let rec aux l = 
    match l with 
    |[] -> 0
    |t::q when (is_kayles t)-> Nim_func.add (Kayles.nim t) (aux q)
    |t::q -> 
      begin 
        let nimber = ref 0 in (*0 élément neutre des nimbers *)
        let j = ref 0 in (*emplacement du debut du dernier jeu pris en compte*)
        let n = Array.length board in 
        let length_t = Array.length t in
        for i = 0 to (n-1) do 
          if (board.(i) = 0) then (
            let current_nimber = (nim_naif (Array.sub t !j (i-(!j)))) in
            nimber := Nim_func.add !nimber current_nimber; 
          (*Optimal nimber add en O(1), et nim_kayles_cas_general supposé calculé en 0(1)*)
            j:= i+1)
        done;
        let dernier_nimber = nim_naif (Array.sub t (!j) (length_t-(!j))) in 
        nimber := Nim_func.add (!nimber) dernier_nimber;
        Nim_func.add !nimber (aux q)
      end
  in
  aux (search_pattern board)
  ;;
