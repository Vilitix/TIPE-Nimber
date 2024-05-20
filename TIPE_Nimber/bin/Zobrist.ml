
let hash_table = Hashtbl.create 100000;;
let restore () = 
  let channel = open_in "/home/arthur/Desktop/TIPE/Hash.txt" in
  let rec aux () = 
    try
      let key = int_of_string (input_line channel) in
      let value = int_of_string (input_line channel) in
      Hashtbl.add hash_table key value;
      aux ()
    with
      End_of_file -> ()
  in
  aux ();
  close_in channel;;

let save () = 
  let channel = open_out "/home/arthur/Desktop/TIPE/data_Cram/Hash.txt" in
  Hashtbl.iter (fun key value -> Printf.fprintf channel "%d\n%d\n" key value) hash_table;
  close_out channel;;

(*chaine de bits générés aléatoirement dans un tableau 2D nb de case*2 si case 0 rien sinon pièce*)
(*peut être abusé 64 bits*)
let generate_hash (i,j) =
  let tab_hash = Array.make_matrix (i*j) 2 0 in 
  for i = 0 to (i*j)-1 do
    tab_hash.(i).(0) <- Random.int max_int;
    tab_hash.(i).(1) <- Random.int max_int;
  done;
  tab_hash;;

let store_hash_jeu (i,j) tab =
  let file = "/home/arthur/Desktop/TIPE/data_Cram/" ^ string_of_int i ^ "_" ^ string_of_int j ^ ".txt" in
  let channel = open_out file in
  for i = 0 to (i*j)-1 do
    Printf.fprintf channel "%d\n%d\n" tab.(i).(0) tab.(i).(1);
  done;






let verify_hash file = 
  let channel = open_in file in
  let rec aux () = 
    try
      let key = int_of_string (input_line channel) in
      let value = int_of_string (input_line channel) in
      if Hashtbl.mem hash_table key then
        if Hashtbl.find hash_table key <> value then
          Printf.printf "Error at key %d\n" key
      else
        Printf.printf "Error at key %d\n" key;
      aux ()
    with
      End_of_file -> ()
  in
  aux ();
  close_in channel;;