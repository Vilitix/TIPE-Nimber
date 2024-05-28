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


  (*pour calculer le minimum exclu d'un ensemble O(nlog(n))*)
let mex (x:SS.t):int =
  let rec loop n:int =
    if SS.mem n x then loop (n+1) else n in 
  loop 0




let decomposition (x:int):int array = (* decomposition en puissance de 2 x*)
  let nb = ref x in 
  let tab = Array.make 11 0 in
  for i=10 downto 0 do (*en réalité 64 bit mais les nimbers de tout le projet ne dépasseront pas 10*)
    let diviseur = int_of_float (2.0**(float_of_int i)) in 
    if (!nb >= diviseur) then 
      begin
        tab.(i) <- 1; nb := !nb - diviseur
      end 
    done;
    tab   


let add (x:int) (y:int):int = 
  (* addition en binaire sans retenue par propriété des nimbers peut aussi driectement s'écrire lxor en Ocaml*)
  let tab_x = decomposition x in 
  let tab_y = decomposition y in
  let result = ref 0 in 
  for i=0 to ((Array.length tab_x) -1) do 
   if ((tab_x.(i) + tab_y.(i)) mod 2) = 1 then result := !result + (int_of_float (2.0**(float_of_int i)))
   done; 
   !result


let mat_multi:(int option array array) = Array.make_matrix 100 100 None;;    

let rec multi (x:int) (y:int):int = (*utilisation de mémoïsation pour la multiplication O(x^2*y^2*log(xy)) dans le pire cas si rien de calculer*)
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
