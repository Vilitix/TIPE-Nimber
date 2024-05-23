
let hash_table = Hashtbl.create 100000;;

let extract_key line =
  Printf.printf "done\n";
  match String.split_on_char ';' line with
  | [ij; hash] ->
    let i, j = match String.split_on_char ',' ij with
      | [i; j] -> int_of_string i, int_of_string j
      | _ -> failwith "format mauvais"
    in
    i, j, hash
  | _->failwith "format mauvais"

let restore () = 
  let channel = open_in "/home/arthur/Desktop/TIPE/data_Cram/Hash.txt" in
  let rec aux () = 
    try
      let raw_key =  (input_line channel) in
      let i,j,hash = extract_key raw_key in
      let key = (i,j,Int32.of_int (int_of_string hash)) in
      let value = Int32.of_int (int_of_string (input_line channel)) in
      Hashtbl.add hash_table key value;
      aux ()
    with
      End_of_file -> ()
  in
  aux ();
  close_in channel;;

let save () = 
  let channel = open_out_gen [Open_append] 0o644 "/home/arthur/Desktop/TIPE/data_Cram/Hash.txt" in
  Hashtbl.iter (fun (i,j,hash) (value:int32) -> Printf.fprintf channel "%d,%d;%li\n%li\n" i j hash value) hash_table;
  close_out channel;;

let save_single hash nimber = 
  let channel = open_out_gen [Open_append] 0o644 "/home/arthur/Desktop/TIPE/data_Cram/Hash.txt" in
  Printf.fprintf channel "%d\n%d\n" hash nimber;
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
  let tab = Array.make_matrix ((i*j)) 2 0l in 
  if (i<= j)&&(i!=0)&&(j!=0) then 
    begin
    if not(Sys.file_exists file)   then
      let res = generate_hash_table (i,j) in
      store_hash_table (i,j) res;
      res
    else
      let channel = open_in file in
      for k = 0 to (i*j)-1 do
        tab.(k).(0) <- Int32.of_int (int_of_string (input_line channel));
        tab.(k).(1) <- Int32.of_int (int_of_string (input_line channel));
      done;
      close_in channel;
      tab
    end
    
  else Array.make_matrix ((i*j)+1) 2 0l;; (*cas pour initialiser la matrice de hash_table dans Cram_reso*)
;;










let init_hash table = 
  let hash = ref 0 in 
  let n,p = Projet_Cram.taille table in
  let tab_hash = get_hash_table (n,p) in
  for i = 0 to (n*p)-1 do
    let l,m = Projet_Cram.tab_to_mat i p in
    if table.(l).(m) = true then
    hash:= !hash lxor (Int32.to_int (tab_hash.(i).(1)))
    else hash := !hash lxor (Int32.to_int (tab_hash.(i).(0)));
  done;
  !hash
;;
