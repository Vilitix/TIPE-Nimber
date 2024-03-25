
exception Beta_cutoff of (((int * int * int)option)*int*int);;
exception Alpha_cutoff of (((int * int * int)option)*int*int);;


let rec alpha_beta table alpha beta depth joueur = 
  try
    let alpha' = ref alpha in
    let beta' = ref beta in 
    let n,p = Projet_Cram.taille table in
    let tab_direction = [|-1;1;-2;2|] in
    let playable = ref 0 in
    let meilleur_coup = ref (None) in
    let meilleur_score = ref (if joueur = 0 then min_int else max_int) in
    let compteur = ref 0 in
    if depth = 0 then 
      failwith "trop profond" (*pour l'instant pg autre probleme*)
    else
      match joueur with
      | 0 ->
        let current_score = ref (min_int) in
        for k = 0 to 3 do
          for i = 0 to (n-1) do
            for j = 0 to (p-1) do
              if (Projet_Cram.is_playable table i j tab_direction.(k)) then
                begin
                  table.(i).(j) <- true;
                  let l,m = Projet_Cram.deuxieme_cases_vise i j tab_direction.(k) in
                  table.(l).(m) <- true;
                  playable := 1;
                  if !current_score < !beta' then
                    begin
                    
                      let _,count,score = alpha_beta table !alpha' !beta' (depth-1) (1-joueur) in
                      table.(i).(j) <- false;
                      table.(l).(m) <- false;
                      compteur := !compteur + count; (*cmpt*)
                      (
                      if score > !current_score then 
                        begin
                          meilleur_score := score;
                          meilleur_coup := Some (i,j,tab_direction.(k));
                      (*Printf.printf "nouveau meilleur coup pour le joueur 0 : %d %d %d score : %d\n" i j tab_direction.(k) score;*)
                          current_score := score;
                          if !current_score > !beta' then 
                            begin
                        
                              raise (Beta_cutoff (Some(i,j,tab_direction.(k)),!compteur,!current_score)) 
                        
                            end;
                        end
                        );
                    
                      alpha' := max !alpha' !current_score;
                    end;
                    table.(i).(j) <- false; (*pour assurer que c'est remis a false si jamais on a pas la premiere condition*)
                    table.(l).(m) <- false;
                end
              done;
            done;
          done;
          if !playable = 0 then
            None,1,-1
          else
            !meilleur_coup,!compteur,!meilleur_score
            
            
      | 1 -> 
        let current_score = ref (max_int) in
          for k = 0 to 3 do 
            for i = 0 to (n-1) do 
              for j = 0 to (p-1) do 
                if (Projet_Cram.is_playable table i j tab_direction.(k)) then 
                  begin
                    table.(i).(j) <- true;
                    let l,m = Projet_Cram.deuxieme_cases_vise i j tab_direction.(k) in
                    table.(l).(m) <- true;
                    playable := 1;
                      
                    if !current_score > !alpha' then
                        
                      begin
                          
                        let _,count,score = alpha_beta table !alpha' !beta' (depth-1) (1-joueur) in
                        table.(i).(j) <- false;
                        table.(l).(m) <- false;
                        compteur := !compteur + count; (*cmpt*)
                        if score < !current_score then 
                          begin
                          meilleur_score := score;
                          meilleur_coup := Some (i,j,tab_direction.(k));
                          (*Printf.printf "nouveau meilleur coup pour le joueur 1 : %d %d %d score : %d\n" i j tab_direction.(k) score;*)
                          current_score := score;
                          if !current_score < !alpha' then 
                            begin
                              
                              raise (Alpha_cutoff (Some(i,j,tab_direction.(k)),!compteur,!current_score))
                              
                            end;
                          end;
                          beta' := min !beta' !current_score;
                      end;
                      table.(i).(j) <- false;
                      table.(l).(m) <- false;
                  end
                done;
              done;
            done;
          if !playable = 0 then
            None,!compteur, 1
          else
            !meilleur_coup,!compteur,!meilleur_score 
      |_ -> failwith "erreur dans le joueur"
  with
  | Beta_cutoff (x,y,z) -> x,y,z
  | Alpha_cutoff (x,y,z) -> x,y,z 
              ;;             



let rec minmax table joueur = 
  let n,p = Projet_Cram.taille table in
  let tab_direction = [|-1;1;-2;2|] in
  let meilleur_score = ref (if joueur = 0 then min_int else max_int) in
  let meilleur_coup = ref (None) in
  let playable = ref 0 in
  for k = 0 to 3 do 
    for i = 0 to (n-1) do 
      for j = 0 to (p-1) do
        if (Projet_Cram.is_playable table i j tab_direction.(k)) then 
          begin
            table.(i).(j) <- true;
            let l,m = Projet_Cram.deuxieme_cases_vise i j tab_direction.(k) in
            table.(l).(m) <- true;
            playable := 1;
            let _,score = minmax table (1-joueur) in
            match joueur with
            |0 -> 
              begin
                table.(i).(j) <- false;
                table.(l).(m) <- false;
                if score > !meilleur_score then 
                  meilleur_score := score;
                  meilleur_coup := Some (i,j,tab_direction.(k));
              end
            |1 ->
              begin
                table.(i).(j) <- false;
                table.(l).(m) <- false;
                if score < !meilleur_score then 
                  meilleur_score := score;
                  meilleur_coup := Some (i,j,tab_direction.(k));
                  
              end;
            |_-> failwith "erreur dans le joueur"
  
          end
      done;
    done;
  done;
  if !playable = 0 then
    if joueur = 0 then None, 1
    else None, -1
  else
  !meilleur_coup, !meilleur_score;;
  

let random_strat table =
  let n,p = Projet_Cram.taille table in
  let tab_direction = [|-1;1;-2;2|] in
  let i = ref (Random.int n) in 
  let j = ref (Random.int p) in
  let k = ref (Random.int 4) in
  while not(Projet_Cram.is_playable table !i !j tab_direction.(!k)) do
          begin
            i := Random.int n;
            j := Random.int p;
            k := Random.int 4;
          end
  done;
!i,!j,tab_direction.(!k)
;;

  let play_and_print_alpha_beta_vs_random table =
    while not (Projet_Cram.perdu table) do  
      (*
      let meilleur_coup, score = minmax table 0 in 
      *)
      
      let depth = 500 in 
      let meilleur_coup,nb_etat,score = alpha_beta table min_int max_int depth 0 in
      Printf.printf "\n nb d'etats explores %d\n" nb_etat;
      
      match meilleur_coup with
      
      | None -> failwith "erreur il n y a pas de coup ou il a perdu"
      | Some (i, j, k) ->
        begin
          table.(i).(j) <- true;
          let l, m = Projet_Cram.deuxieme_cases_vise i j k in
          table.(l).(m) <- true;
          Printf.printf "meilleur coup : %d %d %d, score = %d \n" i j k score;
          Projet_Cram.print_matrix table;
        end;
        if not( Projet_Cram.perdu table) then
          begin
            let coup0, coup1, coup2 =  (random_strat table) in
            Printf.printf "coup random : %d %d %d\n" coup0 coup1 coup2;
            table.(coup0).(coup1) <- true;
            let l, m = Projet_Cram.deuxieme_cases_vise coup0 coup1 coup2 in
            table.(l).(m) <- true;
            Projet_Cram.print_matrix table;
          end
    done
  ;;