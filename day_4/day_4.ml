#use "topfind"
#require "str"
open Printf
open Str

let day = "4"
let in_file = "day_" ^ day ^ "/day_" ^ day ^ ".in";;
let out_file_1 = "day_" ^ day ^ "/day_" ^ day ^"_1.out";;
let out_file_2 = "day_" ^ day ^ "/day_" ^ day ^ "_2.out";;


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

(* Changes a list so that every list element contains all information from one document *)
let rec unpack lst temp result = match lst with
  | [] -> temp :: result
  | x :: rest -> if x = "" then unpack rest "" (temp :: result) else unpack rest (temp ^ " " ^ x) result



let rec contains_all s1 lst =
  let contains s1 s2 =        (* From stack overflow: https://stackoverflow.com/questions/11193783/ocaml-strings-and-substrings/11206238 *)
    try
      let len = String.length s2 in
      for i = 0 to String.length s1 - len do
        if String.sub s1 i len = s2 then raise Exit
      done;
      false
    with Exit -> true
  in
  match lst with
    | [] -> true
    | x :: rest -> contains s1 x && contains_all s1 rest

(* List of mandatory categories for a passport *)
let mandatory = ["byr:"; "iyr:"; "eyr:"; "hgt:"; "hcl:"; "ecl:"; "pid:"]

(* Naloga 1*)
let rec count_valid lst = match lst with 
    | [] -> 0
    | x :: rest -> if contains_all x mandatory then 1 + count_valid rest else count_valid rest;;


(*Naloga 2*)

let rec count_valid_2 lst = 
  let sub_list str = match String.split_on_char ' ' str with
    | "" :: rest -> rest
    | _ -> failwith "Napačen vnos"
  in
  let to_tuple str = match String.split_on_char ':' str with
    | key :: value :: [] -> key, value
    | _ -> failwith "Napačen vnos"
  in
  let hcl_regexp = Str.regexp "^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$" in (* Had some issues with {n} so I used this instead *)
  let ecl_list = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] in
  let pid_regexp = Str.regexp "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$" in (* Same issue with {n} *)
  (* Checks one field *)
  let check_valid_field (key, value) = match key with
    | "byr" -> if int_of_string value <= 2002 then int_of_string value >= 1920 else false
    | "iyr" -> if int_of_string value <= 2020 then int_of_string value >= 2010 else false
    | "eyr" -> if int_of_string value <= 2030 then int_of_string value >= 2020 else false
    | "hgt" -> if String.sub value ((String.length value) - 2) 2 = "cm" && String.length value = 5 then int_of_string (String.sub value 0 3) <= 193 && int_of_string (String.sub value 0 3) >= 150
          else if String.sub value ((String.length value) - 2) 2 = "in" && String.length value = 4 then int_of_string (String.sub value 0 2) <= 76 && int_of_string (String.sub value 0 2) >= 59
          else false
    | "hcl" -> Str.string_match hcl_regexp value 0
    | "ecl" -> List.mem value ecl_list
    | "pid" -> Str.string_match pid_regexp value 0
    | "cid" -> true
    | _ -> failwith "Napaka"
  in
  (* Checks all fields *)
  let rec valid_passport lst = match lst with 
    | [] -> true
    | x :: rest -> if check_valid_field (to_tuple x) then valid_passport rest else false
  in
  (* Counts valid ones; they also have to contain all mandatory fields *)
  match lst with 
    | [] -> 0
    | x :: rest -> if valid_passport (sub_list x) && contains_all x mandatory then 1 + count_valid_2 rest else count_valid_2 rest


let solution_1 = count_valid (unpack (read_file in_file) "" []);;
let solution_2 = count_valid_2 (unpack (read_file in_file) "" []);;


(* Export *)
let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;