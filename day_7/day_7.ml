#use "topfind"
#require "str"
open Printf
open Str

let day = "7"
let in_file = "day_" ^ day ^ "/day_" ^ day ^ ".in";;
let out_file_1 = "day_" ^ day ^ "/day_" ^ day ^"_1.out";;
let out_file_2 = "day_" ^ day ^ "/day_" ^ day ^ "_2.out";;

let remove lst = match lst with 
  | [] -> failwith "že prazen"
  | x :: rest -> rest

let first_el lst = match lst with
  | [] -> failwith "Empty list"
  | x :: rest -> x

let contains s1 s2 =
  let re = Str.regexp_string s2
  in
    try ignore (Str.search_forward re s1 0); true
    with Not_found -> false

let rec find_list bag tuple_lst = match tuple_lst with
  | [] -> failwith "Bag manjka"
  | (key, value) :: rest -> if key = bag then value else find_list bag rest

(* BFS from stack overflow: https://stackoverflow.com/questions/43169972/elegant-bfs-in-ocaml (It's flawed but it doenst concern us) *)
type 'a graph = Gr of ('a * 'a list) list

let get_neighbors node (Gr g) = 
    try List.assoc node g with Not_found -> []

let bfs start g = 
  let v = Hashtbl.create 100 in (*ideally it should be the # of distinct nodes*)
  let q = Queue.create () in    (*v is for visited nodes*)

  let rec bfs' cur_n acc = 
    get_neighbors cur_n g |>
    List.iter
    (fun n -> 
      try Hashtbl.find v n
      with Not_found -> 
        Hashtbl.add v n ();
        Queue.push n q);
    Hashtbl.add v cur_n (); 
    try bfs' (Queue.pop q) (cur_n::acc)
    with Queue.Empty -> List.rev (cur_n::acc) in
    bfs' start []

(* Stackoverflow za poiskati podseznam od indeksa b do e: https://stackoverflow.com/questions/2710233/how-to-get-a-sub-list-from-a-list-in-ocaml *)
let rec sublist b e l = 
  match l with
    [] -> failwith "sublist"
  | h :: t -> 
     let tail = if e=0 then [] else sublist (b-1) (e-1) t in
     if b>0 then tail else h :: tail

let rec concat lst = 
  let rec acc lst str = match lst with
    | [] -> str
    | x :: rest -> if str = "" then acc rest x else acc rest str ^ " " ^ x
  in acc lst ""
  
(* Import *)
let read_file in_file =
  let lines = ref [] in       (*ref na prazen seznam*)
  let ic = open_in in_file in
  let to_tuple str = 
    let lst = Str.split (Str.regexp " [0-9] ") str in
    match lst with 
      | [] -> failwith "napačen seznam"
      | x :: rest -> 
        let startpoint = concat (sublist 0 1 (String.split_on_char ' ' x)) in
        let bag_list lst =
          let rec acc lst output = match lst with
            | [] -> output
            | x :: rest -> acc rest (concat (sublist 0 1 (String.split_on_char ' ' x)) :: output)
          in acc lst []
        in (startpoint, bag_list rest)
  in
  try
    while true; do 
      lines := to_tuple (input_line ic) :: !lines    (*seznam = naslednja vrstica + seznam, vsaka vrstica pretvorjena v int*)
    done; !lines
  with End_of_file -> 
    close_in ic;
    List.rev !lines;;

let get_nums in_file = 
  let lines = ref [] in       (*ref na prazen seznam*)
  let ic = open_in in_file in
  let num_tuple str = 
    let start = concat (sublist 0 1 (String.split_on_char ' ' str)) in
    let my_reg = Str.regexp "[0-9]+" in
    let rec acc lst output = match lst with
      | [] -> output
      | x :: rest -> let _ = Str.search_forward my_reg x 0 in acc rest (int_of_string (Str.matched_string x) :: output)
    in try (start, acc (String.split_on_char ',' str) [])
      with Not_found -> (start, [])
  in
  try
    while true; do 
      lines := num_tuple (input_line ic) :: !lines    (*seznam = naslednja vrstica + seznam, vsaka vrstica pretvorjena v int*)
    done; !lines
  with End_of_file -> 
    close_in ic;
    List.rev !lines;;

(* Naloga 1 *)
let graph = Gr (read_file in_file)
let bag = "shiny gold"

let count_ways_1 lst = 
  let rec acc lst count = match lst with
    | [] -> count
    | (s, conn) :: rest -> if List.mem bag (bfs s graph) then acc rest (count + 1) else acc rest count
  in acc lst 0
  
(* Naloga 2 *)

let count_bags_2 start =
  let rec acc start =
    let count = ref 1 in
    let bags = ref (find_list start (read_file in_file)) in
    let nums = ref (find_list start (get_nums in_file)) in
    let next_bag = ref "" in
    let next_num = ref 0 in
    while !bags != []; do
      next_bag := first_el !bags;
      next_num := first_el !nums;
      count := !count + (!next_num * (acc !next_bag));
      bags := remove !bags;
      nums := remove !nums
    done; !count
  in acc start;;



let solution_1 = count_ways_1 (read_file in_file) - 1     (* We subtract 1 because one way is to just carry the shiny gold bag outermost, which isn't a valid solution *)
let solution_2 = count_bags_2 "shiny gold" - 1            (* We subtract 1 again because the outermost (shiny gold) bag doesn't count *)


(* Export *)
let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;