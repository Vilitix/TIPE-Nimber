(*affiche l'état de la partie*)
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

(*établit  le nimber du tableau(jeu) donc de la somme des jeux de 1 tas*)
let nimber_add tab = 
  let nb = ref 0 in 
  for i = 0 to ((Array.length tab)- 1) do 
    nb := optimal_nimber_add !nb tab.(i); 
  done;
  !nb 
;;

(*renvoie un couple (nb objet retiré, case tableau) calculer pour mettre l'adversaire dans une position finale si possible (nimber = 0)
  sinon enlève un  objet ou c'est possible (en espérant que l'adversaire fasse une erreur au prochain tour)*)
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

(*vérifie si le tableau est nul = partie terminée *)
let is_tab_null tab = 
  let result = ref true in
  for i = 0 to ((Array.length tab) -1) do 
    if tab.(i) <> 0 then result := false 
    done;
    !result
  ;;

(* jeu a un joueur contre un ordinateur qui joue de manière optimale si possible*)
let marienbad tab =
  affiche_tab tab;
  let joueur_gagne = ref false in
  while not (is_tab_null tab) do 
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
    if is_tab_null tab then joueur_gagne := true 
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
