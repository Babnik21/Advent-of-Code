#use "topfind"
#require "str"
open Printf
open Str

let day = "5"
let in_file = "day_" ^ day ^ "/day_" ^ day ^ ".in";;
let out_file_1 = "day_" ^ day ^ "/day_" ^ day ^"_1.out";;
let out_file_2 = "day_" ^ day ^ "/day_" ^ day ^ "_2.out";;


(* Import *)
let read_file in_file =
  let lines = ref [] in       (*ref na prazen seznam*)
  let ic = open_in in_file in
  let bin_str str = 
    let str = Str.global_replace (Str.regexp "[FL]") "0" str in
    let str = Str.global_replace (Str.regexp "[BR]") "1" str in
    str in
  let tuples str = (String.sub str 0 7, String.sub str 7 3) in
  try
    while true; do 
      lines := tuples (bin_str (input_line ic)) :: !lines    
    done; !lines
  with End_of_file -> 
    close_in ic;
    List.rev !lines ;;


(* Takes two binary row and seat number strings and computes seat id *)
let get_id (row, seat) =
  let explode s =             (* https://stackoverflow.com/questions/10068713/string-to-list-of-char/10069969 ; returns a list of binary numbers*)
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (int_of_char s.[i] - 48 :: l) in           (* V kodni tabeli ima char '0' vrednost 48, '1' pa 49 *)
    exp (String.length s - 1) []
  in
  let rec decimal bin_lst =   (* Returns decimal number from a list that represents a binary number *)
    let rec aux lst total = match lst with
    | [] -> total
    | x :: rest -> aux rest (2 * total + x)
    in aux bin_lst 0
  in
  8 * decimal (explode row) + decimal (explode seat)

(* Funtion that sorts a list; https://ocaml.org/learn/taste.html *)
let rec sort = function
  | [] -> []
  | x :: l -> insert x (sort l)
and insert elem = function
  | [] -> [elem]
  | x :: l -> if elem < x then elem :: x :: l else x :: insert elem l;;

(* 1. naloga: *)
let find_max lst = 
  let rec acc lst maximum = match lst with
    | [] -> maximum
    | x :: rest -> acc rest (max maximum (get_id x))
  in acc lst 0

(* 2. naloga: *)
(* Function returns a sorted list with all of the IDs *)
let get_id_list lst =
  let rec acc lst result = match lst with
    | [] -> result
    | x :: rest -> acc rest (get_id x :: result)
  in sort (acc lst [])
  
(* Finds a gap between 2 seats *)
let rec get_my_seat seat_list = match seat_list with
  | last :: [] -> failwith "Ni sedeža"
  | a :: b :: rest -> if a + 2 = b then a + 1 else get_my_seat (b :: rest)
  | _ -> failwith "neki ni kul"

let solution_1 = find_max (read_file in_file)
let solution_2 = get_my_seat (get_id_list (read_file in_file))


(* Export *)
let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;