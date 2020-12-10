open Printf

let day = "10"
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
    List.sort compare !lines ;;

(* Naloga 1 *)
let count_diffs lst =
  let rec helper lst ones threes prev_el = match lst with
    | [] -> ones * (threes + 1)
    | x :: rest -> if x - prev_el = 1 then helper rest (ones + 1) threes x 
                    else if x - prev_el = 3 then helper rest ones (threes + 1) x
                    else helper rest ones threes x
    in helper lst 0 0 0


(* The following function would compute the result but it's not efficient enough. If we split our list into sublists however, it might work.
    We are splitting on each instance where 2 subsequent integers have a difference of 3, and then use the function on the sublists *)
let count_list lst =
  let rec helper lst count prev_el = match lst with
    | [] -> 1
    | x :: [] -> 1
    | x :: y :: [] -> if y - prev_el <= 3 then 2 else 1
    | x :: y :: z :: rest -> if z - prev_el <= 3 then (helper (y :: z :: rest) count x) + (helper (z :: rest) count y) + (helper rest count z)
                              else if y - prev_el <= 3 then (helper (z :: rest) count y) + (helper (y :: z :: rest) count x)
                              else helper (y :: z :: rest) count x
  in helper lst 0 0

let razdeli lst =
  let rec acc lst sublist prev_el current_lst = match lst with
    | [] -> sublist @ [current_lst]
    | x :: rest -> if x - prev_el = 3 then acc rest (sublist @ [current_lst]) x [x]
                    else acc rest sublist x (current_lst @ [x])
  in acc lst [] 0 []

(* Naloga 2 *)
let count_ways lst =
  let rec acc lst prod = match lst with
    | [] -> prod
    | x :: rest -> acc rest (prod * count_list x)
  in acc (razdeli lst) 1




let solution_1 = count_diffs (read_file in_file)
let solution_2 = count_ways (read_file in_file)



(* Export *)

let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;