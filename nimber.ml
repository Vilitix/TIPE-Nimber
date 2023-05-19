



let inverse_tab:(int option array) = Array.make 15 None;; 

(*
let rec inverse x = 
  if inverse_tab.(x) <> None then (extract inverse_tab.(x))
  else 
    let set = ref SS.empty in 
    if x = 1 then (inverse_tab.(x) <- Some 1 ; 1)
    else for 

;;*)




let decomposition x = (* decomposition en puissance de 2 x*)
  let nb = ref x in 
  let tab = Array.make 11 0 in
  for i=10 downto 0 do 
    let diviseur = int_of_float (2.0**(float_of_int i)) in 
    if (!nb >= diviseur) then 
      begin
        tab.(i) <- 1; nb := !nb - diviseur
      end 
    done;
    tab   
  ;;

let optimal_nimber_add x y = (* addition en binaire sans retenue par propriété des nimbers*)
  let tab_x = decomposition x in 
  let tab_y = decomposition y in
  let result = ref 0 in 
  for i=0 to ((Array.length tab_x) -1) do 
   if ((tab_x.(i) + tab_y.(i)) mod 2) = 1 then result := !result + (int_of_float (2.0**(float_of_int i)))
   done; 
   !result
  ;;


(*MARIENBAD*)

let affiche_tab tab = 
  print_string "Les tas sont dans l'ordre numéroté de 0 à ";
  print_int (Array.length tab -1);
  print_string " et il sont de valeur";
  print_string " : ";
  for i = 0 to ((Array.length tab) -2) do 
    print_int (tab.(i));
    print_string ", ";
  done;
  print_int (tab.(Array.length tab - 1));
  print_string ";\n";
;;


let nimber_add tab = 
  let nb = ref 0 in 
  for i = 0 to ((Array.length tab)- 1) do 
    nb := optimal_nimber_add !nb tab.(i); 
  done;
  !nb 
;;

let choisir_coup_parfait tab = 
  let nb = nimber_add tab in 
  let result = ref (tab.(0), 0) in 
  for i = 0 to ((Array.length tab) - 1) do 
    if nb <= tab.(i) then result := (nb, i)
    done;
    let x1, y1 = !result in
    if x1 = 0  then 
      begin
      for i = 0 to ((Array.length tab) - 1) do 
        if not(tab.(i) = 0) then result:= (1, i)
      done;
      !result
      end
    else
      !result
;;

let tab_null tab = 
  let result = ref true in
  for i = 0 to ((Array.length tab) -1) do 
    if tab.(i) <> 0 then result := false 
    done;
    !result
  ;;
let marienbad tab =
  affiche_tab tab;
  let joueur_gagne = ref false in
  while not (tab_null tab) do 
    print_string "Entre le numéro du tas dans lequel tu veux jouer: ";
    flush stdout;
    let n = ref (int_of_string (read_line ())) in

    while ((!n > (Array.length tab - 1)) || (tab.(!n) = 0)) do
      print_string "Ce tas n'existe pas ou est vide. Entre un autre numéro: ";
      flush stdout;
      n := int_of_string (read_line ());
    done;

    print_string "Entre le nombre d'objet que tu veux retirer: ";
    flush stdout;
    let m = ref (int_of_string (read_line ())) in

    while !m > tab.(!n) do 
      print_string "Tu ne peux pas retirer plus d'objet qu'il y en a dans le tas. Choisis un autre nombre: ";
      flush stdout;
      m := int_of_string (read_line ());
    done;

    tab.(!n) <- tab.(!n) - !m;
    affiche_tab tab;
    if tab_null tab then joueur_gagne := true 
    else 
      let nb, num_tas = choisir_coup_parfait tab in 
      print_string "\n";
      print_string "L'ordinateur a enlevé ";
      print_int nb;
      print_string " objet(s) dans le tas numéro ";
      print_int num_tas;
      print_string "\n";
      tab.(num_tas) <- tab.(num_tas) - nb;
      affiche_tab tab;
      print_string "\n";

  done;
  
  if !joueur_gagne then print_string "Bravo, tu as gagné!"
  else print_string "Dommage, l'ordinateur a gagné."
;;



let affichage_unit n = 
  print_string " +  |";
    for i = 0 to n do 
    Printf.printf " %2d " i;
  done;
  print_endline "";

print_string "_____";
  for j = 0 to n do 
    print_string "____";
  done;
  print_endline "";


  
  for k = 0 to n do 
    Printf.printf " %2d |" k;
    for l = 0 to n do 
      Printf.printf " %2d " (optimal_nimber_add k l);
    done;
    print_endline "";
  done;
;;


let affichage_string_add n = 
  let l1 ():string =
    let l = ref " +  |" in
    for i=0 to n do 
      if (String.length (string_of_int i)) < 2 
        then l:= !l ^ " " ^ (string_of_int i) ^ "  "
      else l:= !l ^ " " ^ (string_of_int i) ^ " ";
    done;
    l:= !l ^ "\n";
    l:= !l ^ "_____";
    for j = 0 to n do 
      l:= !l ^ "____";
    done;
    l:= !l ^ "\n";
    !l
  in

  let ligne_centrale () = 
    let l = ref "" in
    for i = 0 to n do 

      if (String.length (string_of_int i)) < 2 
        then l:= !l ^ " " ^ (string_of_int i) ^ "  |"
      else l:= !l ^ " " ^ (string_of_int i) ^ " |";
  
      for j = 0 to n do 
        let nb = optimal_nimber_add i j in 
        if (String.length (string_of_int nb)) < 2 
          then l := !l ^ " " ^ (string_of_int nb) ^ "  "
        else l := !l ^ " " ^ (string_of_int nb) ^ " ";
      done;
      l:= !l ^ "\n"
    done;
    !l
  in
  (l1 ()) ^ (ligne_centrale ())
;;

let affichage_string_multi n = 
  let l1 ():string =
    let l = ref " ·   |" in
    for i=0 to n do 
      if (String.length (string_of_int i)) = 3 
        then l:= !l ^ " " ^ (string_of_int i) ^ " "
      else if (String.length (string_of_int i)) = 2 
            then l:= !l ^ " " ^ (string_of_int i) ^ "  "
          else l:= !l ^ " " ^ (string_of_int i) ^ "   ";
    done;
    l:= !l ^ "\n";
    l:= !l ^ "_____";
    for j = 0 to n do 
      l:= !l ^ "_____";
    done;
    l:= !l ^ "\n";
    !l
  in

  let ligne_centrale () = 
    let l = ref "" in
    for i = 0 to n do 
      if (String.length (string_of_int i)) = 3
        then l:= !l ^ " " ^ (string_of_int i) ^ " |"
      else if (String.length (string_of_int i)) = 2
            then l:= !l ^ " " ^ (string_of_int i) ^ "  |"
          else l:= !l ^ " " ^ (string_of_int i) ^ "   |";
  
      for j = 0 to n do 
        let nb = nimber_multi_dynamique i j in 
        if (String.length (string_of_int nb)) = 3 
          then l := !l ^ " " ^ (string_of_int nb) ^ " "
        else if (String.length (string_of_int nb)) = 2 
          then l := !l ^ " " ^ (string_of_int nb) ^ "  "
          else l := !l ^ " " ^ (string_of_int nb) ^ "   ";
      done;
      l:= !l ^ "\n"
    done;
    !l
  in
  (l1 ()) ^ (ligne_centrale ())
;;



let () = 
let channel = open_out "/home/arthur/Desktop/TIPE/sortie.txt" in
 Printf.fprintf channel "%s\n" (affichage_string_add 99);
close_out channel;;



nimber_addition 3 7;;
nimber_multi_dynamique 2 2;;


let set_ex = SS.empty;;
let set_ex = SS.add 3 set_ex;;

let set_ex = SS.remove 3 set_ex;;
SS.mem 3 set_ex;;
SS.iter (fun x -> print_int x; print_string " ") set_ex;;
calcul_mex set_ex;;
