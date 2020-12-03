open Printf

let in_file = "day_3/day_3.in";;
let out_file_1 = "day_3/day_3_1.out";;
let out_file_2 = "day_3/day_3_2.out";;


(* Import *)
let read_file in_file =
  let lines = ref [] in       (*ref na prazen seznam*)
  let ic = open_in in_file in
  try
    while true; do 
      lines := input_line ic :: !lines    
    done; !lines
  with End_of_file -> 
    close_in ic;
    List.rev !lines ;;


(* Rešitev naloge 1: precej preprosto, samo štejemo *)

let rec count_trees_inline list_1 step start skip = 
  if skip = 1 then
    match list_1 with
    | [] -> 0
    | x :: [] -> if x.[start] = '#' then 1 else 0
    | x :: skipped :: rest -> if x.[start] = '#' then 1 + count_trees_inline rest step ((start + step) mod String.length x) skip else count_trees_inline rest step ((start + step) mod String.length x) skip
    else 
    match list_1 with 
    | [] -> 0
    | x :: rest -> if x.[start] = '#' then 1 + count_trees_inline rest step ((start + step) mod String.length x) skip else count_trees_inline rest step ((start + step) mod String.length x) skip
  
(* Rešitev 2. naloge: zmnožimo med seboj, ko pridemo do konca še primer ko preskakujemo vsako drugo vrstico *)

let rec prod_trees list_1 traj_list = match traj_list with
  | [] -> count_trees_inline list_1 1 0 1
  | x :: rest -> (count_trees_inline list_1 x 0 0) * prod_trees list_1 rest 

let solution_1 = count_trees_inline (read_file in_file) 3 0 0
let solution_2 = prod_trees (read_file in_file) [1; 3; 5; 7]



(* Export *)
let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;