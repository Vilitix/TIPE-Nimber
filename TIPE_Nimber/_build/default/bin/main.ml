


let table = [|[|false;false;false;false;false;false|];[|false;false;false;false;false;false|];[|false;false;false;false;false;false|];[|false;false;false;false;false;false|];[|false;false;false;false;false;false|]|];;

let uf = Projet_Cram.init_uf table;;
let uf,_ = Projet_Cram.play table uf 0 1 2;;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table ;;

let uf,_ = Projet_Cram.play table uf 1 1 1;;
let () = Projet_Cram.print_matrix table;;

let () = Projet_Cram.print_uf uf table ;;
let uf,_ = Projet_Cram.play table uf 3 1 (-2);;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table ;; 
let uf,_ = Projet_Cram.play table uf 3 3 (-2);;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table ;; 

let uf,_ = Projet_Cram.play table uf 1 2 (-2);;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table ;; 

let uf,_ = Projet_Cram.play table uf 0 5 (1);;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table ;; 
let uf,tab_opt = Projet_Cram.play table uf 2 4 (-1);;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table;; 

let l = Projet_Cram.tab_post_sep table uf (Option.get tab_opt);; 
let rec afficher l = 
  match l with 
  |[] -> ()
  |tab::q -> Projet_Cram.print_matrix tab; afficher q
;;
afficher l;; 

let table2 = [|[|false;false;false|];[|false;false;false|];[|false;false;false|]|];;
Printf.printf "nimber_moins_naif = %d \n" (Projet_Cram.nimber_exact_moins_naif table2);;
Printf.printf "nimber_naif = %d \n" (Projet_Cram.nimber_exact_naif table2);;
if (not(Projet_Cram.resultat_couple table2 0)) then Printf.printf "nimber_final est bon"



