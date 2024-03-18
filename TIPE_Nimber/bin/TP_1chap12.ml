let borne = 10000;;

let f s = 
  for i = 0 to borne -1 do 
    print_string s;
    Printf.printf "%d " i;
  done
  Thread.exit ()
;;

let use_thread = 
  