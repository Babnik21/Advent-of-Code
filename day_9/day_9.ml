#use "topfind"
#require "str"
open Printf
open Str

let day = "9"
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

(* Check if 2 numbers sum to desired sum *)
let rec check_sum lst_1 lst_2 sum = 
  let rec helper lst num sum = match lst with
    | [] -> false
    | x :: rest -> if x + num = sum && x != num then true else helper rest num sum
  in
  match lst_1 with
    | [] -> false
    | x :: rest -> if helper lst_2 x sum then true else check_sum rest lst_2 sum

(* Naloga 1 *)
let find_error lst =
  let rec helper lst pool = match lst with
    | [] -> failwith "Ni napake"
    | x :: rest when List.length pool < 25 -> helper rest (pool @ [x])
    | x :: rest -> if check_sum pool pool x then helper rest (List.tl pool @ [x]) else x
  in helper lst []

(* Helper functions*)
let list_sum lst = List.fold_left (+) 0 lst
let min_plus_max lst = 
  let sorted = List.sort compare lst
  in List.hd sorted + List.hd (List.rev sorted)

(* Naloga 2 *)
let find_error_2 lst sum = 
  let rec helper lst sum pool = match lst with
    | [] -> failwith "Error"
    | x :: rest when list_sum pool > sum -> helper lst sum (List.tl pool)
    | x :: rest when list_sum pool < sum -> helper rest sum (pool @ [x])
    | x :: rest when list_sum pool = sum -> min_plus_max pool
    | _ -> failwith "Napaka"
  in helper lst sum []


let solution_1 = in_file |> read_file |> find_error
let solution_2 = find_error_2 (read_file in_file) solution_1


(* Export *)
let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;