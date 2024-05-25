
(*
Random.self_init ();;


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
let uf,tab_opt = Projet_Cram.play table uf 4 0 (-1);;
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
(*
Printf.printf "\n--------------------------\n";;
let table2 = Array.make_matrix 3 3 false;;
Printf.printf "\nnimber = %d \n" (Projet_Cram.nimber_exact_naif table2);;
Cram_alpha_beta.play_and_print_alpha_beta_vs_random table2;;
*)

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

(*
let tab_try = [|
    Array.make_matrix 1 1 false;
    Array.make_matrix 1 2 false;
    Array.make_matrix 1 3 false;
    Array.make_matrix 2 3 false;
    Array.make_matrix 3 4 false|]

let len_tab tab = (Array.length tab) * (Array.length tab.(0));;


let measure_time f len tab = 
  let channel = open_out "/home/arthur/Desktop/TIPE/sortie_time.txt" in
  for i = 0 to Array.length tab - 1 do
    let t1 = Sys.time () in
    ignore (f tab.(i));
    let t2 = Sys.time () in
    Printf.fprintf channel "%d : %f\n" (len tab.(i))  (t2 -. t1) ; (*taille entrée : temps*);
  done;
  close_out channel;
;;
*)
(*
let copy =Zobrist.restore ();; 
(*
Printf.printf("%d\n") (Cram_reso.nimber_non_naif [|[|false;true;true;true|];[|true;true;false;false|];[|true;false;false;false|]|]);;
Printf.printf("%d\n") (Projet_Cram.nimber_exact_naif [|[|false;true;true;true|];[|true;true;false;false|];[|true;false;false;false|]|]);;
*)




let vrai_test_all_tab n m = 
  let tab = Array.make_matrix n m false in
  let rec test_tab i j k =
    if i = -1 then ()
    else
      begin
        ignore (Cram_reso.nimber_non_naif tab);
        let newi, newj, newk = Projet_Cram.iter tab (i, j, k) in
        Printf.printf "i = %d j = %d k = %d, n %d, m %d\n \n" i j k n m;
        if Projet_Cram.is_playable tab i j k then
          begin
            let t, l = Projet_Cram.deuxieme_cases_vise i j k in
            tab.(i).(j) <- true;
            tab.(t).(l) <- true;
            ignore (Cram_reso.nimber_non_naif tab);
            test_tab newi newj newk
          end
        else
          test_tab newi newj newk
      end
  in
  test_tab (n - 1) 0 (-2)
;;





(*
  let test_iter table = 
    let n,p = Projet_Cram.taille table in 
    let i = ref (n/2) in
    let k = ref (-1) in 
     (*sinon out of bound initialement*)
    let j = ref (p/2) in 
    while !i<>(-1) do 
      (if Projet_Cram.is_playable table !i !j !k then
        (
      let v,l = Projet_Cram.deuxieme_cases_vise !i !j !k in
      table.(!i).(!j) <-true ;
      table.(v).(l) <- true;
      Projet_Cram.print_matrix table;
      table.(v).(l) <- false;
      table.(!i).(!j) <- false;
        ));
      let newi,newj,newk = Cram_alpha_beta.iter_heur table (!i,!j,!k) in
        i := newi;
        j := newj;
        k := newk;
    done;;
  let table = [|[|false;false;false;false;false|]|];;
  test_iter table;;
  Printf.printf "%d \n" (Projet_Cram.nimber_exact_moins_naif table);;
  *)
  
for i = 1 to 5 do 
  for j = i to 5 do 
    vrai_test_all_tab i j;
  done;
done
;;




  
Zobrist.save copy;;
*)
let copy = Zobrist.restore ();;
let table = [|[|false;false;false|];[|false;false;false|]|];;

Printf.printf "nimber table : %d" (Cram_reso.nimber_non_naif table);;
Zobrist.save copy;;