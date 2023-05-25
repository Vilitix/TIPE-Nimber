module SS = Set.Make(Int);;
(*     SS.empty: O(1)
    SS.add: O(log n)
    SS.mem: O(log n)
    SS.remove: O(log n)
    SS.union: O(n log n)
    SS.inter: O(n log n)
    SS.diff: O(n log n)
    SS.fold: O(n)
    SS.iter: O(n)
 *)

(*FONCTIONS AUXILIAIRE *)
let fermats_nb = [|2;4;16;256;65536|];;

let est_fermat_nb x =
  let rep = ref false in
  for i=0 to ((Array.length fermats_nb)- 1) do 
    if x = fermats_nb.(i) then rep:= true
    done;
    !rep
;;

let closest_fermat_nb n = (*affiche le nombre de fermat inférieur le plus proche *)
  if n = 0 || n= 1 then n
  else
  let rec loop i =
  if fermats_nb.(i) >= n then fermats_nb.(max (i-1) 0) else loop (i + 1)  in
  loop 0  
;;

  (*pour calculer le minimum exclu d'un ensemble O(nlog(n))*)
let calcul_mex x =
  let rec loop n:int =
    if SS.mem n x then loop (n+1) else n in 
  loop 0
;;

let extract opt =
  match opt with
  | None -> failwith "Rien à extraire"
  | Some x -> x
;;


(*POUR LA MULTIPLICATION*)
let decomposition x = (* decomposition en puissance de 2 x*)
  let nb = ref x in 
  let tab = Array.make 11 0 in
  for i=10 downto 0 do 
    let diviseur = int_of_float (2.0**(float_of_int i)) in 
    if (!nb >= diviseur) then 
      begin
        tab.(i) <- 1; nb := !nb - diviseur
      end 
    done;
    tab   
  ;;

let optimal_nimber_add x y = (* addition en binaire sans retenue par propriété des nimbers*)
  let tab_x = decomposition x in 
  let tab_y = decomposition y in
  let result = ref 0 in 
  for i=0 to ((Array.length tab_x) -1) do 
   if ((tab_x.(i) + tab_y.(i)) mod 2) = 1 then result := !result + (int_of_float (2.0**(float_of_int i)))
   done; 
   !result
  ;;


(* FIN FONCTION AUXILIAIRES*)


  (*nimber addition récursif naif *)
let rec nimber_addition (x:int) (y:int):int =
  let set = ref SS.empty in (* complexité O(1) *)
  if x = 0 then y
  else if y = 0 then x
  else if x = y then 0 
  else
    begin
      for i=0 to y-1 do
        set:= SS.union !set  (SS.singleton (nimber_addition i x)) (*complexité ((|Set|+Nimberadd)ln|Set|+Nimberadd) *)
      done;
      for j=0 to x-1 do
        set:= SS.union !set (SS.singleton (nimber_addition j y)) 
      done;
    
      calcul_mex !set
      end
;;




  let rec optimal_nimber_multi a b = 
    let f_a = closest_fermat_nb a in
    let a_1 = a/f_a in 
    let a_2 = a mod f_a in 
    let f_b = closest_fermat_nb b in 
    let b_1 = b/f_b in 
    let b_2 = b mod f_a in 
    if (est_fermat_nb a) && (est_fermat_nb b) then 
      a*b
    else 
      if (f_a < f_b) then 
    nimber_addition ((optimal_nimber_multi a b_1)*(f_b))  (optimal_nimber_multi a b_2)
      else if (f_b < f_a) then 
    nimber_addition ((optimal_nimber_multi b a_1)*(f_a))  (optimal_nimber_multi b a_2)
  else 
    let p_1 = optimal_nimber_multi a_1 b_1 in
    let p_2 = optimal_nimber_multi a_2 b_2 in
    let p_3 = optimal_nimber_multi (nimber_addition a_1 a_2) (nimber_addition b_1 b_2) in
    let p_4 = optimal_nimber_multi p_1 (f_a/2) in 
    let p_5 = nimber_addition p_3 p_2 in
    nimber_addition  (p_5*f_a) (nimber_addition p_2 p_4)
  ;;

let mat_multi:(int option array array) = Array.make_matrix 100 100 None;;    

let rec nimber_multi_memo x y = 
  if mat_multi.(x).(y) <> None then (extract mat_multi.(x).(y))
  else  
    let set = ref SS.empty in 
    if x = 1 then (mat_multi.(x).(y) <- Some y; y)
    else if y = 1 then  (mat_multi.(x).(y) <- Some x; x)
    else if x = 0 || y = 0 then (mat_multi.(x).(y) <- Some 0;0)
    else
      begin 
        for i = 0 to y-1 do
          for j= 0 to x-1 do
            set := SS.union !set (SS.singleton (optimal_nimber_add (optimal_nimber_add (nimber_multi_memo i x)  (nimber_multi_memo y j)) (nimber_multi_memo i j)))
          done;
        done;
        let result = calcul_mex !set in
        mat_multi.(x).(y) <- Some result; result
        
      end 
;;

(*https://math.stackexchange.com/questions/1671933/nimber-multiplication*)