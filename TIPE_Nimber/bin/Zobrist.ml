
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
  let channel = open_out "/home/arthur/Desktop/TIPE/Hash.txt" in
  Hashtbl.iter (fun key value -> Printf.fprintf channel "%d\n%d\n" key value) hash_table;
  close_out channel;;

