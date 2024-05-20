module Unionfind = Struct_pers.Make (Struct_pers.New_Arr);;
let cap = 50;;


let resultat_couple table nimber uf= 
  let n,p = Projet_Cram.taille table in
    let tab_direction = [|-1;1;-2;2|] in  
    let rec resultat_couple_aux table nimber uf = 
      let playable = ref false in
      let res = ref false in
      for k = 0 to 3 do 
        for i = 0 to (n-1) do 
          for j = 0 to (p-1) do 

            if (Projet_Cram.is_playable table i j tab_direction.(k)) then 
              begin
                table.(i).(j) <- true;
                let l,m = Projet_Cram.deuxieme_cases_vise i j tab_direction.(k) in
                table.(l).(m) <- true;
                playable := true;
                let new_uf, new_tab_c = Projet_Cram.actualiser_union_find table uf i j tab_direction.(k) in
                begin
                  match new_tab_c with
                  |None -> (if not(resultat_couple_aux table nimber new_uf) then res:=true);
                  |Some tab_c -> 
                    begin

                      let tab_post_sep = Projet_Cram.tab_post_sep table new_uf tab_c in
                      match tab_post_sep with 
                      |[] -> failwith "appel impossible à la fonction" 
                      |t::q ->
                        begin
                          let new_nimber = ref nimber in
                          (*appel au dernier algorithme fonction refaite pour problème d'
                             interdépendance*)
                          let calcul_nimber_final table = 
                            let i = ref 0 in
                            let uf = Projet_Cram.init_uf table in
                            while (not(resultat_couple_aux table !i uf ) && (!i<= cap) ) do 
                              i := !i+1
                            done;
                            if !i = (cap) then failwith "le nimber est supérieur au max précisé"
                            else !i
                          in
                          List.iter (fun sous_table -> new_nimber := !new_nimber lxor (calcul_nimber_final sous_table)) q; 
                          if not(resultat_couple_aux t !new_nimber (Projet_Cram.init_uf t)) then res:=true
                        end

                    end
                end;

                
                table.(i).(j) <- false;
                table.(l).(m) <- false;
              end
          done;
        done;
      done;
      if ((nimber = 0) && (not(!playable))) then false
      else
        begin

        for i = 0 to nimber -1 do
          if not(resultat_couple_aux table i uf) then res:= true
          done;

      (*toutes les options sont perdantes car pas d'exceptions*)
        !res
        end 
    in 
    resultat_couple_aux table nimber uf
  ;;
  
  let nimber_non_naif table = 
    let i = ref 0 in
    let uf = Projet_Cram.init_uf table in
    while ((resultat_couple table !i uf ) && (!i<= cap) ) do 
      i := !i+1
    done;
    if !i = (cap) then failwith "le nimber est supérieur au max précisé"
    else !i
  ;;
