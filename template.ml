open Printf

let in_file = "day_2/day_2.in";;
let out_file_1 = "day_2/day_2_1.out";;
let out_file_2 = "day_2/day_2_2.out";;


(* Import *)
let () = 
  let ic = open_in in_file in
  (* Uvozimo naše podatke iz .in datoteke *)
  close_in ic;;









(* Export *)
let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;