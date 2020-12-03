open Printf

let day = "1"
let in_file = "day_" ^ day ^ "/day_" ^ day ^ ".in";;
let out_file_1 = "day_" ^ day ^ "/day_" ^ day ^"_1.out";;
let out_file_2 = "day_" ^ day ^ "/day_" ^ day ^ "_2.out";;


(* Import *)
let read_file in_file =
  let lines = ref [] in       (*ref na prazen seznam*)
  let ic = open_in in_file in
  try
    while true; do 
      lines := int_of_string(input_line ic) :: !lines    (*seznam = naslednja vrstica + seznam, vsaka vrstica pretvorjena v int*)
    done; !lines
  with End_of_file -> 
    close_in ic;
    List.rev !lines ;;




let solution_1 = 1
let solution_2 = 2



(* Export *)
let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;