open Printf

let in_file = "day_1.in";;
let sporocilo = "Hello";;
let out_file = "day_1_1.out";;

let () = 
  let oc = open_out out_file in
  fprintf oc "%s\n" sporocilo;
  close_out oc;

