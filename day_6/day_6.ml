open Printf

let day = "6"
let in_file = "day_" ^ day ^ "/day_" ^ day ^ ".in";;
let out_file_1 = "day_" ^ day ^ "/day_" ^ day ^"_1.out";;
let out_file_2 = "day_" ^ day ^ "/day_" ^ day ^ "_2.out";;

(* Helper *)
let explode s =                         (* explode: https://stackoverflow.com/questions/10068713/string-to-list-of-char/10069969 *)
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* Import *)
let read_file in_file =
  let lines = ref [] in       (*ref na prazen seznam*)
  let ic = open_in in_file in
  try
    while true; do 
      lines := input_line ic :: !lines    (*seznam = naslednja vrstica + seznam, vsaka vrstica pretvorjena v int*)
    done; !lines
  with End_of_file -> 
    close_in ic;
    List.rev !lines ;;

let group_str lst naloga = 
  if naloga = 1 then 
    let rec acc lst current str = match lst with
      | [] -> str :: current
      | x :: rest -> if x = "" then acc rest (str :: current) "" else acc rest current (str ^ x)
    in acc lst [] ""
  else 
    let rec acc lst current str = match lst with
      | [] -> String.sub str 1 (String.length str - 1) :: current
      | x :: rest -> if x = "" then acc rest (String.sub str 1 (String.length str - 1) :: current) "" else acc rest current (str ^ "," ^ x)
    in acc lst [] ""

(* Naloga 1 *)
let count_distinct str = 
  let rec acc char_lst distinct_lst = match char_lst with
    | [] -> List.length distinct_lst
    | x :: rest -> if List.mem x distinct_lst then acc rest distinct_lst else acc rest (x :: distinct_lst)
  in acc (explode str) []


let sum_distinct lst =
  let rec counter_1 lst count = match lst with
    | [] -> count
    | x :: rest -> counter_1 rest (count + count_distinct x)
  in counter_1 (group_str lst 1) 0

(* Naloga 2 *)
let count_constant str =
  let to_tuples str = match String.split_on_char ',' str with
    | [] -> failwith "Prazno"
    | x :: rest -> explode x , rest
  in
  let rec appears_everytime char_1 lst = match lst with
    | [] -> true
    | x :: rest -> if String.contains x char_1 then appears_everytime char_1 rest else false
  in
  let rec acc (char_lst , str_lst) count = match char_lst with
    | [] -> count
    | x :: rest -> if appears_everytime x str_lst then acc (rest, str_lst) 1 + count else acc (rest, str_lst) count
  in acc (to_tuples str) 0

let sum_constant lst = 
  let rec counter_2 lst count = match lst with
    | [] -> count
    | x :: rest -> counter_2 rest (count + count_constant x)
  in counter_2 (group_str lst 2) 0


let solution_1 = sum_distinct (read_file in_file)
let solution_2 = sum_constant (read_file in_file)



(* Export *)
let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;