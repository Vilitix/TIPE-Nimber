module Unionfind = Struct_pers.Make (Struct_pers.New_Arr);;
let cap = 50;; (*le nimber est généralement très inférieur à 50 et une limite est nécessaire pour l'algorithme principal*)

let mat_hash = Array.init 19 (fun i -> Array.init 19 (fun j -> Zobrist.get_hash_table (i,j) ));;
(*on génère au préalable nos tables pre-enregistrées (pour ne pas avoir des valeurs différentes de hachage à
   chaque exécution) pour calculer le hachage de Zobrist *)



let rec resultat_couple table nimber uf (hash:int) = 
  let n,p = Projet_Cram.taille table in
  assert (n<=p); (*pour eviter les symétries on se ramène toujours à n <= p même si la table est séparée*)
  let res = Hashtbl.find_opt Zobrist.hash_table (n,p,hash) in

  if res != None then begin(((Option.get res)) != nimber )end
  else
  
  begin
    let playable = ref false in
    let res = ref false in
    
    let i = ref (n/2) in
    let k = ref (-1) in 
    let j = ref (p/2) in 
    while ((!i <> -1) && (!res != true) && (n*p != 1)) do 
      if (Projet_Cram.is_playable table !i !j !k) then 
        begin
        table.(!i).(!j) <- true;
        let l,m = Projet_Cram.deuxieme_cases_vise !i !j !k in
        table.(l).(m) <- true;
        let new_hash = hash lxor mat_hash.(n).(p).((!i*p) + !j ).(0) lxor mat_hash.(n).(p).((!i*p) + !j).(1)
        lxor mat_hash.(n).(p).(l*p + m).(0) lxor mat_hash.(n).(p).(l*p+m).(1) in
        playable := true;
        let new_uf, new_tab_c = Projet_Cram.actualiser_union_find table uf !i !j !k in
        begin
          match new_tab_c with
          |_ when !res = true -> () (*éviter des appels récursifs inutiles*)
          |None -> (if not(resultat_couple table nimber new_uf new_hash ) then res:=true );
          (*une option perdante rend le résultat gagnant*)
          |Some tab_c -> 
            begin
              (*tab_c est un tableau persistant appartenant à la structure de l'union find qui est 
                 renvoyé seulement si une séparation est detecté *)
              let tab_post_sep = Projet_Cram.tab_post_sep table new_uf tab_c in
              (*listes des tableaux de composantes indépendantes du jeu*)
              match tab_post_sep with 
              |[] -> failwith "la fonction tab_post_sep est appelé sur un état séparable\n" 
              |t::q ->
                begin
                  let new_nimber = ref nimber in
                  (*appel au dernier algorithme fonction refaite pour problème d'
                  interdépendance*)
                  let calcul_nimber_final table_fun = 
                    let i = ref 0 in
                    let nfun,pfun = Projet_Cram.taille table_fun in
                    let hash_intern = Zobrist.init_hash table_fun in
                    if not(Hashtbl.mem Zobrist.hash_table (n,p,hash))then 
                      begin
                      let uf_intern = Projet_Cram.init_uf table_fun in
                      
                      while ((resultat_couple table_fun !i uf_intern hash_intern ) && (!i<= cap) ) do 
                        i := !i+1
                      done;
                      if !i = cap then failwith "le nimber est supérieur au max précisé\n"
                      else !i
                  end
                else (Hashtbl.find Zobrist.hash_table (nfun,pfun,hash_intern)) 
              in
              List.iter (fun sous_table -> new_nimber := !new_nimber lxor (calcul_nimber_final sous_table)) q; 
              if not((resultat_couple t !new_nimber (Projet_Cram.init_uf t) (Zobrist.init_hash t))) then res:=true 
              end (*si une option perdante alors l'étape en cours est gagnante*)
              
            end;
          end;
          
          
          table.(!i).(!j) <- false;
          table.(l).(m) <- false;
        end;
        (
        let newi,newj,newk = Cram_alpha_beta.iter_heur table (!i,!j,!k) in
        i := newi;
        j := newj;
        k := newk;
        )
      done;
      
      if ((nimber = 0) && (not(!playable))) then begin  Hashtbl.add Zobrist.hash_table (n,p,hash) 0;false  end
        (*c'est le cas de base de notre algorithme *)
    else
      begin
        
        if !res then true
        else
        begin
          for i = 0 to nimber -1 do
            if not(resultat_couple table i uf hash ) then res:= true
              (*appel sur les options, une option perdante suffit*)
            done;
          if !res = false then  
            begin 
              Hashtbl.add Zobrist.hash_table (n,p,hash) (nimber);
              (*c'est une propriété*)
              !res 
            end
          else true

        end 
      end
      
      
    end
  ;;
  


(*
let rec resultat_couple table nimber uf (hash:int) = 
  Printf.printf "appel nimber %d  et table :\n" nimber;
  Projet_Cram.print_matrix table;
  let n,p = Projet_Cram.taille table in
  assert (n<=p); (*pour eviter les symétries*)
  let res = Hashtbl.find_opt Zobrist.hash_table (n,p,hash) in
  let print_option r = 
    match r with 
    |None -> Printf.printf "pas dans la table\n"
    |Some x -> Printf.printf "deja vu %d\n" (x)
  in
  print_option res; 
  
  
  if res != None then begin (((Option.get res)) != nimber ) end
  else
  
  begin
    Printf.printf "passe ? \n";
    let playable = ref false in
    let res = ref false in
    let i = ref (n/2) in
    let k = ref (-1) in 
    let j = ref (p/2) in 
    Printf.printf "appel i %d j %d k %d\n" !i !j !k;
    while ((!i <> -1) && (!res != true) && (n*p != 1)) do
      if (Projet_Cram.is_playable table !i !j !k) then 
        (*il faut jouer le meilleur coup pour avoir une option perdante qui suffit pour terminer l'algorithme d'ou 
      l'utilisation du alpha beta *)
      begin


    
        Printf.printf "coup %d %d %d\n" !i !j !k;
        table.(!i).(!j) <- true;
        let l,m = Projet_Cram.deuxieme_cases_vise !i !j !k in
        table.(l).(m) <- true;
        Projet_Cram.print_matrix table;
        let new_hash = hash lxor mat_hash.(n).(p).((!i*p) + !j ).(0) lxor mat_hash.(n).(p).((!i*p) + !j).(1)
        lxor mat_hash.(n).(p).(l*p + m).(0) lxor mat_hash.(n).(p).(l*p+m).(1) in
        playable := true;
        let new_uf, new_tab_c = Projet_Cram.actualiser_union_find table uf !i !j !k in
        begin
          match new_tab_c with
          |_ when !res = true -> () (*éviter des appels récursifs inutiles*)
          |None ->Printf.printf "appel\n";(if not(resultat_couple table nimber new_uf new_hash ) then res:=true);
          |Some tab_c -> 
            begin
              Printf.printf "appel 2\n";
              let tab_post_sep = Projet_Cram.tab_post_sep table new_uf tab_c in
              match tab_post_sep with 
              |[] -> failwith "appel impossible à la fonction\n" 
              |t::q ->
                begin
                  let new_nimber = ref nimber in
                  (*appel au dernier algorithme fonction refaite pour problème d'
                  interdépendance des fonctions*)
                  let calcul_nimber_final table_fun = 
                    let i = ref 0 in
                    let hash = Zobrist.init_hash table_fun in
                    let uf = Projet_Cram.init_uf table_fun in
                    while (not(resultat_couple table_fun !i uf hash ) && (!i<= cap) ) do 
                      i := !i+1
                    done;
                    if !i = (cap) then failwith "le nimber est supérieur au max précisé\n"
                    else !i
                in
                List.iter (fun sous_table -> new_nimber := !new_nimber lxor (calcul_nimber_final sous_table)) q; 
                if (resultat_couple t !new_nimber (Projet_Cram.init_uf t) (Zobrist.init_hash t)) then res:=true
                end
              
            end;
          end;
          
          
          table.(!i).(!j) <- false;
          table.(l).(m) <- false;
        end;
        (
        Printf.printf "appel alpha beta\n";
        let coup,_,_ = Cram_alpha_beta.alpha_beta_coup table !i !j !k 10 in 
        (*valeur de depth peut être non optimale*)
        (*coups fait 2 fois boucle infini*)
        
        match coup with 
        |None -> (i := -1);Printf.printf "fin de la partie\n"
        |Some (a,b,c) -> (i := a; j := b; k := c)
        ;
        
        )
      done;
      
      if ((nimber = 0) && (not(!playable))) then begin Printf.printf "ajout nimber : %d\n" nimber;Projet_Cram.print_matrix table;Hashtbl.add Zobrist.hash_table (n,p,hash) 0;false end
      else
      begin
        
        if !res then true
        else
        begin
          
          for i = 0 to nimber -1 do
            if not(resultat_couple table i uf hash ) then res:= true
            done;
          if !res = false then  
            begin 
            Printf.printf "ajout nimber : %d\n" nimber;Projet_Cram.print_matrix table;  Hashtbl.add Zobrist.hash_table (n,p,hash) (nimber); 
            !res 
          end
        else true
        
      end 
    end
    
    
  end
;;
*)


let nimber_non_naif table = 
  let n,p = Projet_Cram.taille table in
  let i = ref 0 in
  let hash = Zobrist.init_hash table in
  if not(Hashtbl.mem Zobrist.hash_table (n,p,hash))then 
    begin
      let uf = Projet_Cram.init_uf table in
      while ((resultat_couple table !i uf hash ) && (!i<= (cap)) ) do 
        i := !i+1;
      done;
      if !i = (cap+1) then failwith "le nimber est superieur au max precise"
      else !i 
    end
  else (Hashtbl.find Zobrist.hash_table (n,p,hash))
;;
