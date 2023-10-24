

let print_int_array arr =
  Array.iter (fun x -> print_int x; print_string " ") arr;
  print_newline ()

let rec print_int_array_list lst =
  match lst with
  | [] -> ()
  | hd::tl -> 
    print_int_array hd;
    print_int_array_list tl
	;;
print_int (Twopins.nim_naif [|2;1;1;2;2;2;1;2;2|]);
print_int (Twopins.nim [|2;1;1;2;2;2;1;2;2|]);
