

let somme_tab tab = 
  let x = ref 0 in 
  for i = 0 to ((Array.length tab ) -1 ) do 
    x:= !x + tab.(i);
  done;
  !x
;;

let recopie_label_nimber nb channel tab = 
  for i=0 to ((Array.length tab ) -1) do 
    if (not(String.length (tab.(i)) == 0)) then 
      Printf.fprintf channel "    %d -> %d [label = \"%s\"];\n" nb i tab.(i);
  done;
;;
let graph_marienbad tab = 
  let nb = nimber_tab tab in
  let tab_label_nimber = Array.make ((somme_tab tab) + nb + 1) "" in 
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
    for j = 1 to tab.(i) do
      let tab_copie = Array.copy tab in 
      tab_copie.(i) <- tab_copie.(i) - j;
      tab_label_nimber.(nimber_tab tab_copie) <- (tab_label_nimber.(nimber_tab tab_copie)) ^ "\\n" ^ "tas(" ^ (string_of_int i) ^ ") - " ^ (string_of_int j);
    done;
  done;
  recopie_label_nimber nb channel tab_label_nimber;
  Printf.fprintf channel "}";
close_out channel;
ignore (Sys.command "dot -Tpng -o /home/arthur/Desktop/TIPE/sortie.png /home/arthur/Desktop/TIPE/sortie.txt");
;;

let graph_end_marienbad file = 
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
  ignore (Sys.command "dot -Tpng -o /home/arthur/Desktop/TIPE/sortie.png /home/arthur/Desktop/TIPE/sortie.txt");
;;




  

(*Soit faire un tableau avec les occurrences de chaque nimber pour ne pas surchargé le graph et mettre plus dans les labels des sommets 
   soit bourrin tout *)

(*afficher le graph des coup de la partie a la fin avec en couleur un joueur en couleur l autre*)

   let tab = [|1;2|] in
let incremente tab ;;
