module Unionfind = Struct_pers.Make (Struct_pers.New_Arr);;
let cap = 50;;

let mat_hash = Array.init 19 (fun i -> Array.init 19 (fun j -> Zobrist.get_hash_table (i,j) ));; 



let rec resultat_couple table nimber uf (hash:int) = 
  let n,p = Projet_Cram.taille table in
  assert (n<=p); (*pour eviter les symétries*)
  let res = Hashtbl.find_opt Zobrist.hash_table (n,p,Int32.of_int hash) in
  (*let print_option r = 
    match r with 
    |None -> Printf.printf "pas dans la table\n"
    |Some x -> Printf.printf "deja vu %d\n" (Int32.to_int x)
  in
  print_option res; 
  *)
  
  if res != None then ((Int32.to_int (Option.get res)) != nimber )
  else
  
  
  begin
    let playable = ref false in
    let res = ref false in
    let i = ref (n-1) in
    let j = ref 0 in 
    let k = ref (-2) in 
    while ((!i <> -1) && (!res != true) && (n*p != 1)) do 
      if (Projet_Cram.is_playable table !i !j !k) then 
        begin
        table.(!i).(!j) <- true;
        let l,m = Projet_Cram.deuxieme_cases_vise !i !j !k in
        table.(l).(m) <- true;
        let new_hash = hash lxor (Int32.to_int mat_hash.(n).(p).((!i*p) + !j ).(0)) lxor (Int32.to_int mat_hash.(n).(p).((!i*p) + !j).(1))
        lxor (Int32.to_int mat_hash.(n).(p).(l*p + m).(0)) lxor (Int32.to_int mat_hash.(n).(p).(l*p+m).(1)) in
        playable := true;
        let new_uf, new_tab_c = Projet_Cram.actualiser_union_find table uf !i !j !k in
        begin
          match new_tab_c with
          |_ when !res = true -> () (*éviter des appels récursifs inutiles*)
          |None -> (if not(resultat_couple table nimber new_uf new_hash ) then res:=true);
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
                  interdépendance*)
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
                if not(resultat_couple t !new_nimber (Projet_Cram.init_uf t) (Zobrist.init_hash t)) then res:=true
                end
              
            end;
          end;
          
          
          table.(!i).(!j) <- false;
          table.(l).(m) <- false;
        end;
        (
        let newi,newj,newk = Projet_Cram.iter table (!i,!j,!k) in
        i := newi;
        j := newj;
        k := newk;
        )
      done;
      
      if ((nimber = 0) && (not(!playable))) then begin Hashtbl.add Zobrist.hash_table (n,p,Int32.of_int hash) 0l;false end
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
              Hashtbl.add Zobrist.hash_table (n,p,Int32.of_int hash) (Int32.of_int nimber); 
              !res 
            end
          else true

        end 
      end
      
      
    end
  ;;
  


let nimber_non_naif table = 
  let n,p = Projet_Cram.taille table in
  let i = ref 0 in
  let hash = Zobrist.init_hash table in
  if not(Hashtbl.mem Zobrist.hash_table (n,p,Int32.of_int hash))then 
    begin
      let uf = Projet_Cram.init_uf table in
      while ((resultat_couple table !i uf hash ) && (!i<= (cap)) ) do 
        i := !i+1;
      done;
      if !i = (cap+1) then failwith "le nimber est superieur au max precise"
      else begin Printf.printf "add dans non naif\n"; Hashtbl.add Zobrist.hash_table (n,p,Int32.of_int hash) (Int32.of_int !i);!i end
    end
  else Int32.to_int (Hashtbl.find Zobrist.hash_table (n,p,Int32.of_int hash))
;;
