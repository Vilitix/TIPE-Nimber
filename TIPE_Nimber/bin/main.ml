Random.self_init ();;

(*
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
*)

Printf.printf "\n--------------------------\n";;
let table2 = Array.make_matrix 3 4 false;;
Printf.printf "\nnimber = %d \n" (Projet_Cram.nimber_exact_naif table2);;
Projet_Cram.play_and_print_alpha_beta_vs_random table2;;

(*
let table2 = [|[|false;false;false;false|];[|false;false;false;false|];[|false;false;false;false|];|];;
Printf.printf "nimber_moins_naif = %d \n" (Projet_Cram.nimber_exact_moins_naif table2);;
Printf.printf "nimber_naif = %d \n" (Projet_Cram.nimber_exact_naif table2);;
Printf.printf "nimber_pas_naif = %d \n" (Projet_Cram.nimber_non_naif table2 )
let _,_ = Projet_Cram.play table2 uf 1 0 (-2);;
let _,_ = Projet_Cram.play table2 uf 0 1 (-2);;



let i,j,k = Option.get (fst (Projet_Cram.alpha_beta table2 (ref (min_int)) (ref max_int) 800 0));;
Printf.printf "\nle joueur 1 joue et veut perdre\n";
Printf.printf "i = %d j = %d k = %d \n" i j k;;
Printf.printf "\n score = %d\n";;
let _,_ = Projet_Cram.play table2 uf i j (k);;
let () = Projet_Cram.print_matrix table2;;
*)
(*
let i,j,k = Option.get (fst (Projet_Cram.alpha_beta table2 (ref (min_int)) (ref max_int) 800 0));;
Printf.printf "i = %d j = %d k = %d \n" i j k;;
let _,_ = Projet_Cram.play table2 uf i j (k);;
let () = Projet_Cram.print_matrix table2;;

let i,j,k = Option.get (fst (Projet_Cram.alpha_beta table2 (ref (min_int)) (ref max_int) 800 0));;
Printf.printf "i = %d j = %d k = %d \n" i j k;;
let _,_ = Projet_Cram.play table2 uf i j (k);;
let () = Projet_Cram.print_matrix table2;;

let i,j,k = Option.get (fst (Projet_Cram.alpha_beta table2 (ref (min_int)) (ref max_int) 800 0));;
Printf.printf "i = %d j = %d k = %d \n" i j k;;
let _,_ = Projet_Cram.play table2 uf i j (k);;
let () = Projet_Cram.print_matrix table2;;
*)
(*
let play_test_a_b table = 
  while true do
  match fst (Projet_Cram.alpha_beta table (ref (-max_int)) (ref max_int) 30 0) with 
  |None -> Printf.printf "Erreur il y a un problème \n"
  |Some (i,j,direction) -> 
      if (Projet_Cram.is_playable table i j direction) then 
        begin
        table.(i).(j) <- true;
        let k,l = Projet_Cram.deuxieme_cases_vise i j direction in 
        table.(k).(l) <- true;
        end
      else failwith "coup incorrect"
      ;

  Projet_Cram.print_matrix table;
  let i1,j1,k1 = int_of_string (read_line ()) , int_of_string (read_line ()) , int_of_string (read_line ()) in
  flush stdout;
  if (Projet_Cram.is_playable table i j direction) then 
    begin
    table.(i1).(j1) <- true;
    let k,l = Projet_Cram.deuxieme_cases_vise i1 j1 k1 in 
    table.(k).(l) <- true;
    end
  else failwith "coup incorrect"
  ;
  Projet_Cram.print_matrix table;

  done;
;;

play_test_a_b table2;;

*)
