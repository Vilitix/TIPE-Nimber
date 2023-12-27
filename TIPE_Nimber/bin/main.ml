


let table = [|[|false;false;false;false;false;false|];[|false;false;false;false;false;false|];[|false;false;false;false;false;false|];[|false;false;false;false;false;false|];[|false;false;false;false;false;false|]|];;

let uf = Projet_Cram.init_uf table;;
let uf = Projet_Cram.play uf table 0 1 2;;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table ;;

let uf = Projet_Cram.play uf table 1 1 1;;
let () = Projet_Cram.print_matrix table;;

let () = Projet_Cram.print_uf uf table ;;
let uf = Projet_Cram.play uf table 3 1 (-2);;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table ;; 
let uf = Projet_Cram.play uf table 3 3 (-2);;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table ;; 

let uf = Projet_Cram.play uf table 1 2 (-2);;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table ;; 

let uf = Projet_Cram.play uf table 0 5 (1);;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table ;; 
let uf = Projet_Cram.play uf table 2 4 (-1);;
let () = Projet_Cram.print_matrix table;;
let () = Projet_Cram.print_uf uf table ;; 
let l = Projet_Cram.tab_post_sep table uf;; 
let rec afficher l = 
  match l with 
  |[] -> ()
  |tab::q -> Projet_Cram.print_matrix tab; afficher q
;;
afficher l;;