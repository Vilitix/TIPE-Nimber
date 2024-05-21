(*affiche l'état de la partie*)
let affiche_tab tab =
  print_string "Les tas sont dans l'ordre numéroté de 0 à ";
  print_int (Array.length tab - 1);
  print_string " et il sont de valeur";
  print_string " : ";
  for i = 0 to Array.length tab - 2 do
    print_int tab.(i);
    print_string ", "
  done;
  print_int tab.(Array.length tab - 1);
  print_string ";\n"


let somme_tab tab = (*pour la taille maximum du tableau de nimber *)
let x = ref 0 in 
for i = 0 to ((Array.length tab ) -1 ) do 
  x:= !x + tab.(i);
done;
!x

(*établit  le nimber du tableau(jeu) donc de la somme des jeux de 1 tas*)
let nimber_tab tab =
  let nb = ref 0 in
  for i = 0 to Array.length tab - 1 do
    nb := Nim_func.add !nb tab.(i)
  done;
  !nb

(*renvoie un couple (nb objet retiré, case tableau) calculer pour mettre l'adversaire dans une position finale si possible (nimber = 0)
  sinon enlève un  objet ou c'est possible aléatoirement (en espérant que l'adversaire fasse une erreur au prochain tour)*)
let choisir_coup_parfait tab =
  let nb = nimber_tab tab in
  let result = ref (tab.(0), 0) in
  for i = 0 to Array.length tab - 1 do
    if nb <= tab.(i) then result := (nb, i)
  done;
  let x1, _ = !result in
  if x1 = tab.(0) || x1 = 0 then (
    let i = ref (Random.int (Array.length tab)) in
    while tab.(!i) = 0 do
      i := Random.int (Array.length tab)
    done;
    result := (Random.int tab.(!i) + 1, !i);
    !result)
  else !result

(*vérifie si le tableau est nul = partie terminée *)
let is_tab_null tab =
  let result = ref true in
  for i = 0 to Array.length tab - 1 do
    if tab.(i) <> 0 then result := false
  done;
  !result
;;

let recopie_label_nimber nb channel tab = (*permet d'ajouter des noeuds et de les nommer*)
for i=0 to ((Array.length tab ) -1) do 
  if (not(String.length (tab.(i)) == 0)) then 
    Printf.fprintf channel "    %d -> %d [label = \"%s\"];\n" nb i tab.(i);
done;
;;

let graphe tab = 
  let nb = nimber_tab tab in
  let tab_label_nimber = Array.make ((somme_tab tab) + nb + 1) "" in (*pour créer un tableau assez grand des nimbers possible de coup+position actuelle*)
  let channel = open_out "/home/arthur/Desktop/TIPE/sortie.txt" in
  Printf.fprintf channel "digraph G {
    fontname=\"Helvetica,Arial,sans-serif\"
    node [fontname=\"Helvetica,Arial,sans-serif\"]
    edge [fontname=\"Helvetica,Arial,sans-serif\"]
    rankdir = LR;
    nodesep = 1;\n\n";
  Printf.fprintf channel "    subgraph cluster {
		      style=rounded;
		      color=lightgrey;
		      node [shape = doublecircle];
		      %d
		      label = \"position actuelle\";}\n\n" (nb);
  
  for i = 0 to ((Array.length tab) -1) do 
    for j = 1 to tab.(i) do (*boucle affichant tous les coups possibles dans le graphe avec leur nimber*)
      let tab_copie = Array.copy tab in 
      tab_copie.(i) <- tab_copie.(i) - j;
      tab_label_nimber.(nimber_tab tab_copie) <- (tab_label_nimber.(nimber_tab tab_copie)) ^ "\\n" ^ "tas(" ^ (string_of_int i) ^ ") - " ^ (string_of_int j);
    done;
  done;
  recopie_label_nimber nb channel tab_label_nimber;
  Printf.fprintf channel "}";
ignore (Sys.command "dot -Tpng -o /home/arthur/Desktop/TIPE/sortie.png /home/arthur/Desktop/TIPE/sortie.txt"); (*execution pour actualiser le graphe après un coup*)
;;


let graphe_end file = (*graphe finale de l'affichage de tous les coups de la partie*)
  let channel = open_out "/home/arthur/Desktop/TIPE/sortie.txt" in
  Printf.fprintf channel "digraph \"Deroulement de la partie\" {
    fontname=\"Helvetica,Arial,sans-serif\"
    node [fontname=\"Helvetica,Arial,sans-serif\"]
    edge [fontname=\"Helvetica,Arial,sans-serif\"]
    nodesep = 1;\n\n";
  let j = ref (Queue.pop file) in 
  let k = ref 0 in 
  let joueur = ref " joueur" in
  for i = 1 to ((Queue.length file)) do 
    k:= Queue.pop file;
    Printf.fprintf channel "coup_%d_nimber_%d -> coup_%d_nimber_%d [label = \"%s\"];\n" (i-1) (!j) i (!k) !joueur;  
    j:= !k;
    if !joueur = " joueur" then joueur:= " ordi"
    else joueur:= " joueur"
  done;
  Printf.fprintf channel "";
  Printf.fprintf channel "}";
  close_out channel;
  ignore (Sys.command "dot -Tpng -o /home/arthur/Desktop/TIPE/sortie.txt");
;;

(* jeu a un joueur contre un ordinateur qui joue de manière optimale si possible*)
let marienbad tab =
  affiche_tab tab;
  let joueur_gagne = ref false in
  let file_partie = Queue.create () in
  (*enregistrement des coups dans une file pour la fin de partie*)
  ignore (Sys.command "feh --reload=1 /home/arthur/Desktop/TIPE/sortie.png &");
  (*affiche le graphe des coup possible et leur nimber*)
  while not (is_tab_null tab) do
    graphe tab;
    (*actualise le graph*)
    Queue.push (nimber_tab tab) file_partie;
    print_string "Entre le numéro du tas dans lequel tu veux jouer: ";
    flush stdout;
    let n = ref (int_of_string (read_line ())) in

    while !n > Array.length tab - 1 || tab.(!n) = 0 do
      print_string "Ce tas n'existe pas ou est vide. Entre un autre numéro: ";
      flush stdout;
      n := int_of_string (read_line ())
    done;

    print_string "Entre le nombre d'objet que tu veux retirer: ";
    flush stdout;
    let m = ref (int_of_string (read_line ())) in

    while !m > tab.(!n) do
      print_string
        "Tu ne peux pas retirer plus d'objet qu'il y en a dans le tas. Choisis \
         un autre nombre: ";
      flush stdout;
      m := int_of_string (read_line ())
    done;

    tab.(!n) <- tab.(!n) - !m;
    Queue.push (nimber_tab tab) file_partie;
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
      print_string "\n"
  done;
  graphe_end file_partie;
  (*graphe de tous les coups joués par ordi et joueur selon leur nimber*)
  if !joueur_gagne then print_string "Bravo, tu as gagné!"
  else print_string "Dommage, l'ordinateur a gagné."

















