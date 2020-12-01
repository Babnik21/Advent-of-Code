open Printf

let in_file = "day_1/day_1.in";;
let out_file_1 = "day_1/day_1_1.out";;
let out_file_2 = "day_1/day_1_2.out";;


(*Import *)
(* Used code accessible at https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)
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

(*Finds two numbers that add to sum*)
let rec find_sumands num_list sum =
  let rec find_partner num_list sum = match num_list with
    | [] -> []
    | first :: [] -> []
    | first :: second :: rest -> if first + second = sum then [first; second] else find_partner (first :: rest) sum
  in 
  match num_list with
    | [] -> []
    | x :: rest -> if find_partner num_list sum != [] then find_partner num_list sum else find_sumands rest sum



(* Calculates sum of 2 elements in list if list has exactly 2 elements*)
let sum_list list_1 = match list_1 with
  | a :: b :: [] -> a + b
  | a :: b :: c :: [] -> a + b + c
  | [] -> 0
  | _ -> failwith "Napačen seznam"


(*Calculates product of 2 elements in list if list has exactly 2 elements*)
let prod list_1 = match list_1 with
  | a :: b :: [] -> a * b
  | a :: b :: c :: [] -> a * b * c
  | [] -> 1
  | _ -> failwith "Napačen seznam"


(*Solution for problem 1*)
let solution_1 = prod (find_sumands(read_file in_file) 2020);;


(* Finds three numbers that add to sum using the previously written function for that does the same for 2 numbers*)
let rec find_three num_list sum = match num_list with
  | first :: second :: [] -> []
  | first :: [] -> []
  | [] -> []
  | first :: rest -> if first + sum_list (find_sumands rest (sum - first)) = sum then (first :: find_sumands rest (sum - first)) else find_three rest sum

(*Solution for problem 2*)
let solution_2 = prod (find_three (read_file in_file) 2020);;


(* Printing in .out file *)
let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;

