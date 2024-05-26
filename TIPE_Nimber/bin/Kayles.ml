

(*Kayles.ml*)
let (mat_kayles: int option array) = Array.make 100 None;; 
(*on place dans ce tableau mat_kayles[i] = nimber du jeu de Kayles avec n colonnes de 1 quille *)
mat_kayles.(0) <- Some 0;;
mat_kayles.(1) <- Some 1;;


let rec nim_cas_general (n:int):int = 
	(*Nimber de K_{n} si tout est à calculer  n^2log(n^2) sinon O(nlog(n^2)) (n appels)*)
if mat_kayles.(n) <> None then (Option.get mat_kayles.(n))
else 
	let set = ref Nim_func.SS.empty in
	for  i = 0 to (n-1) do 
		set := Nim_func.SS.add (Nim_func.add (nim_cas_general i) (nim_cas_general (n-(i+1)) )) !set; (*enlever 1 quille*)
		if i <> n-1 then
			set := Nim_func.SS.add (Nim_func.add (nim_cas_general i) (nim_cas_general (n-(i+2)) )) !set; (*enlever 2 quilles adjacentes*)
		done;
		mat_kayles.(n) <- Some (Nim_func.mex !set);
		Option.get mat_kayles.(n)
	
		
let nim (board:int option array):int = (*O(n)*)
	let nimber = ref 0 in (*0 élément neutre des nimbers *)
	let j = ref 0 in (*emplacement du debut du dernier jeu pris en compte*)
	let n = Array.length board in 
	for i = 0 to (n-1) do 
		if (board.(i) = 0) then 
			let current_nimber = (nim_cas_general (i - !j)) in
			nimber := Nim_func.add !nimber current_nimber; 
		(*add en O(1) (Xor bit a bit), et nim_cas_general supposé calculé en 0(1)*)
			j:= i+1
		done;
		let dernier_nimber = nim_cas_general (n - (!j)) in 
	 	nimber := Nim_func.add (!nimber) dernier_nimber;
	!nimber

      
        


      
    