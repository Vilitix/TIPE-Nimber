let affichage_unit n = 
  print_string " +  |";
    for i = 0 to n do 
    Printf.printf " %2d " i;
  done;
  print_endline "";

print_string "_____";
  for j = 0 to n do 
    print_string "____";
  done;
  print_endline "";


  
  for k = 0 to n do 
    Printf.printf " %2d |" k;
    for l = 0 to n do 
      Printf.printf " %2d " (optimal_nimber_add k l);
    done;
    print_endline "";
  done;
;;


let affichage_string_add n = 
  let l1 ():string =
    let l = ref " +  |" in
    for i=0 to n do 
      if (String.length (string_of_int i)) < 2 
        then l:= !l ^ " " ^ (string_of_int i) ^ "  "
      else l:= !l ^ " " ^ (string_of_int i) ^ " ";
    done;
    l:= !l ^ "\n";
    l:= !l ^ "_____";
    for j = 0 to n do 
      l:= !l ^ "____";
    done;
    l:= !l ^ "\n";
    !l
  in

  let ligne_centrale () = 
    let l = ref "" in
    for i = 0 to n do 

      if (String.length (string_of_int i)) < 2 
        then l:= !l ^ " " ^ (string_of_int i) ^ "  |"
      else l:= !l ^ " " ^ (string_of_int i) ^ " |";
  
      for j = 0 to n do 
        let nb = optimal_nimber_add i j in 
        if (String.length (string_of_int nb)) < 2 
          then l := !l ^ " " ^ (string_of_int nb) ^ "  "
        else l := !l ^ " " ^ (string_of_int nb) ^ " ";
      done;
      l:= !l ^ "\n"
    done;
    !l
  in
  (l1 ()) ^ (ligne_centrale ())
;;

let affichage_string_multi n = 
  let l1 ():string =
    let l = ref " ·   |" in
    for i=0 to n do 
      if (String.length (string_of_int i)) = 3 
        then l:= !l ^ " " ^ (string_of_int i) ^ " "
      else if (String.length (string_of_int i)) = 2 
            then l:= !l ^ " " ^ (string_of_int i) ^ "  "
          else l:= !l ^ " " ^ (string_of_int i) ^ "   ";
    done;
    l:= !l ^ "\n";
    l:= !l ^ "_____";
    for j = 0 to n do 
      l:= !l ^ "_____";
    done;
    l:= !l ^ "\n";
    !l
  in

  let ligne_centrale () = 
    let l = ref "" in
    for i = 0 to n do 
      if (String.length (string_of_int i)) = 3
        then l:= !l ^ " " ^ (string_of_int i) ^ " |"
      else if (String.length (string_of_int i)) = 2
            then l:= !l ^ " " ^ (string_of_int i) ^ "  |"
          else l:= !l ^ " " ^ (string_of_int i) ^ "   |";
  
      for j = 0 to n do 
        let nb = nimber_multi_dynamique i j in 
        if (String.length (string_of_int nb)) = 3 
          then l := !l ^ " " ^ (string_of_int nb) ^ " "
        else if (String.length (string_of_int nb)) = 2 
          then l := !l ^ " " ^ (string_of_int nb) ^ "  "
          else l := !l ^ " " ^ (string_of_int nb) ^ "   ";
      done;
      l:= !l ^ "\n"
    done;
    !l
  in
  (l1 ()) ^ (ligne_centrale ())
;;

let () = 
let channel = open_out "/home/arthur/Desktop/TIPE/sortie.txt" in
 Printf.fprintf channel "%s\n" (affichage_string_add 99);
close_out channel;;