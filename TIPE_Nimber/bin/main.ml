
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

let table = [|[|true;true;true|];[|true;false;true|];[|true;false;false|];[|true;false;false|];[|true;false;false|]|];;
let tab = Projet_Cram.actualiser_union_find tab;;

Projet_Cram.print_uf tab;;
