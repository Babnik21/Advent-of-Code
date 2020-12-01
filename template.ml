open Printf

let in_file = "day_1.in";;
let out_file_1 = "day_1_1.out";;
let out_file_2 = "day_1_2.out";;



let () = 
  let ic = open_in in_file in
  (* Uvozimo naše podatke iz .in datoteke *)
  close_in ic;;









(* Izpisovanje v file*)
let () = 
  let oc_1 = open_out out_file_1 in
  (* Tle dopisemo kaj zelimo izpisati v out *)
  close_out oc_1;
  let oc_2 = open_out out_file_1 in
  (* Tle dopisemo kaj zelimo izpisati v out *)
  close_out oc_2;

