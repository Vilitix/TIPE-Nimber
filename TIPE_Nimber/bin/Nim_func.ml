(*Nim_func.ml*)
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
    Structure d'Ensembles implémentés avec des AVL
 *)

(*FONCTIONS AUXILIAIRES *)

  (*pour calculer le minimum exclu d'un ensemble O(nlog(n))*)
let mex x =
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

let add x y = (* addition en binaire sans retenue par propriété des nimbers*)
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
let rec add_naif (x:int) (y:int):int =
  let set = ref SS.empty in (* complexité O(1) *)
  if x = 0 then y
  else if y = 0 then x
  else if x = y then 0 
  else
    begin
      for i=0 to y-1 do
        set:= SS.union !set  (SS.singleton (add_naif i x)) 
      done;
      for j=0 to x-1 do
        set:= SS.union !set (SS.singleton (add_naif j y)) 
      done;
    
      mex !set
      end
;;


let mat_multi:(int option array array) = Array.make_matrix 100 100 None;;    

let rec multi x y = (*utilisation de mémoïsation pour la multiplication O(x^2*y^2*log(xy)) dans le pire cas si rien de calculer*)
  if mat_multi.(x).(y) <> None then (Option.get mat_multi.(x).(y)) (* si rien de calculer  *)
  else  
    let set = ref SS.empty in 
    if x = 1 then (mat_multi.(x).(y) <- Some y; y)
    else if y = 1 then  (mat_multi.(x).(y) <- Some x; x)
    else if x = 0 || y = 0 then (mat_multi.(x).(y) <- Some 0;0)
    else
      begin 
        for i = 0 to y-1 do
          for j= 0 to x-1 do (*SS.add O(log(n))  si tout les précedents sont calculés*)
            set := SS.add  (add (add (multi i x)  (multi y j)) (multi i j)) !set
          done;
        done;
        let result = mex !set in (*O(|set|log(|set|))*)
        mat_multi.(x).(y) <- Some result; result
        
      end 
;;
