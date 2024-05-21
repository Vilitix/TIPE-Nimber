
let hash_table = Hashtbl.create 100000;;
let restore () = 
  let channel = open_in "/home/arthur/Desktop/TIPE/Hash.txt" in
  let rec aux () = 
    try
      let key = Int32.of_int (int_of_string (input_line channel)) in
      let value = Int32.of_int (int_of_string (input_line channel)) in
      Hashtbl.add hash_table key value;
      aux ()
    with
      End_of_file -> ()
  in
  aux ();
  close_in channel;;

let save () = 
  let channel = open_out "/home/arthur/Desktop/TIPE/data_Cram/Hash.txt" in
  Hashtbl.iter (fun (key:int32) (value:int32) -> Printf.fprintf channel "%li\n%li\n" key value) hash_table;
  close_out channel;;

(*chaine de bits générés aléatoirement dans un tableau 2D nb de case*2 si case 0 rien sinon pièce*)
let generate_hash_table (i,j) =
  let tab_hash = Array.make_matrix (i*j) 2 0l in 
  for i = 0 to (i*j)-1 do
    tab_hash.(i).(0) <- Random.int32 Int32.max_int;
    tab_hash.(i).(1) <- Random.int32 Int32.max_int; 
  done;
  tab_hash;;

let store_hash_table (i,j) tab =
  let file = "/home/arthur/Desktop/TIPE/data_Cram/" ^ string_of_int i ^ "_" ^ string_of_int j ^ ".txt" in
  let channel = open_out file in
  for i = 0 to (i*j)-1 do
    Printf.fprintf channel "%li\n%li\n" tab.(i).(0) tab.(i).(1);
  done;;

let get_hash_table (i,j) =
  let file = "/home/arthur/Desktop/TIPE/data_Cram/" ^ string_of_int i ^ "_" ^ string_of_int j ^ ".txt" in
  let tab = Array.make_matrix (i*j) 2 0l in
  if not(Sys.file_exists file)  then
    let res = generate_hash_table (i,j) in
    store_hash_table (i,j) res;
    res
  else
    let channel = open_in file in
    for i = 0 to (i*j)-1 do
      tab.(i).(0) <- Int32.of_int (int_of_string (input_line channel));
      tab.(i).(1) <- Int32.of_int (int_of_string (input_line channel));
    done;
    close_in channel;
    tab
  ;;







let verify_hash file = (*recherche si probleme de collisions *)
  let channel = open_in file in
  let rec aux () = 
    try
      let key = Int32.of_int (int_of_string (input_line channel)) in
      let value =Int32.of_int (int_of_string (input_line channel)) in
      if Hashtbl.mem hash_table key then
        if Hashtbl.find hash_table key <> value then
          Printf.printf "Error at key %li\n" key
      else
        Printf.printf "Error at key %li\n" key;
      aux ()
    with
      End_of_file -> ()
  in
  aux ();
  close_in channel;;

