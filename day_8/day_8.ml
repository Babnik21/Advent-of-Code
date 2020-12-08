#use "topfind"
#require "str"
open Printf
open Str

let day = "8"
let in_file = "day_" ^ day ^ "/day_" ^ day ^ ".in";;
let out_file_1 = "day_" ^ day ^ "/day_" ^ day ^"_1.out";;
let out_file_2 = "day_" ^ day ^ "/day_" ^ day ^ "_2.out";;

let el_from_tuple tuple index = match tuple with
  | (x, y) -> if index = 0 then x else y

(* Import *)
let read_file in_file =
  let lines = ref [] in       (*ref na prazen seznam*)
  let ic = open_in in_file in
  let to_tuples str = match String.split_on_char ' ' str with
    | x :: y :: [] -> (x, int_of_string y)
    | _ -> failwith "Napačna vrstica"
  in
  try
    while true; do 
      lines := to_tuples (input_line ic) :: !lines    (*seznam = naslednja vrstica + seznam, vsaka vrstica pretvorjena v int*)
    done; !lines
  with End_of_file -> 
    close_in ic;
    List.rev !lines ;;

(* Naloga 1 *)
let walk lst =
  let rec acc lst index count visited = match List.nth lst index with
    | ("acc", x) -> if List.mem (index + 1) visited then count + x else acc lst (index + 1) (count + x) (index :: visited)
    | ("nop", x) -> if List.mem (index + 1) visited then count else acc lst (index + 1) count (index :: visited)
    | ("jmp", x) -> if List.mem (index + x) visited then count else acc lst (index + x) count (index :: visited)
    | _ -> failwith "Napačen vnos"
  in acc lst 0 0 []

let walk_2 lst =
  let rec acc lst index count visited = match List.nth lst index with
    | ("acc", x) -> if List.nth_opt lst (index + 1) = None then (count + x, 1) 
                    else if List.mem (index + 1) visited then (count + x, 0) else acc lst (index + 1) (count + x) (index :: visited)
    | ("nop", x) -> if List.nth_opt lst (index + 1) = None then (count, 1) 
                    else if List.mem (index + 1) visited then (count, 0) else acc lst (index + 1) count (index :: visited)
    | ("jmp", x) -> if index + x > List.length lst then (count, 0) 
                    else if List.nth_opt lst (index + x) = None then (count, 1) 
                    else if List.mem (index + x) visited then (count, 0) else acc lst (index + x) count (index :: visited)
    | _ -> failwith "Napačen vnos"
  in 
  let rec change lst index count visited = match List.nth lst index with
    | ("acc", x) -> if List.nth_opt lst (index + 1) = None then count + x else change lst (index + 1) (count + x) (index :: visited)     (* If statement shouldn't ever be true but just in case *)
    | ("jmp", x) -> if List.nth_opt lst (index + 1) = None then count
                    else if el_from_tuple (acc lst (index + 1) count (index :: visited)) 1 = 1 then el_from_tuple (acc lst (index + 1) count (index :: visited)) 0
                    else change lst (index + x) count (index :: visited)
    | ("nop", x) -> if List.nth_opt lst (index + x) = None then change lst (index + 1) count (index :: visited)
                    else if el_from_tuple (acc lst (index + x) count (index :: visited)) 1 = 1 then el_from_tuple (acc lst (index + x) count (index :: visited)) 0
                    else change lst (index + 1) count (index :: visited)
    | _ -> failwith "Napačen vnos"
  in change lst 0 0 []

let solution_1 = in_file |> read_file |> walk
let solution_2 = in_file |> read_file |> walk_2



(* Export *)
let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;