open Printf

(* Z uporabo knjižnice Str bi bila rešitev zelo enostavna saj bi lahko z regularnim izrazom razdelil input string na
več kosov in delal direktno od tam. Ker pa nisem znal usposobiti knjižnice sem stringe ločeval na dolg grd način *)

let in_file = "day_2/day_2.in";;
let out_file_1 = "day_2/day_2_1.out";;
let out_file_2 = "day_2/day_2_2.out";;


(* Import *)
(* Uporabil isto funkcijo kot včeraj z nekaj spremembami (dopisal funkcije to_lists, remove_colon, count_chars) 
Toliko časa sem popravljal kako sprocesiram input, da sem že v uvozu praktično rešil nalogo*)
let read_file in_file =
  let lines = ref [] in       (*ref na prazen seznam*)
  let ic = open_in in_file in
  let limits_list str = match String.split_on_char '-' str with     (* Makes a list of limits (int list)*)
    | a :: b :: [] -> [int_of_string a ; int_of_string b]
    | _ -> failwith "Neprimeren input"
  in
  let remove_colon str = match String.split_on_char ':' str with    (* Vrne character, katerega štejemo v passwordu (odstrani dvopičje)*)
    | a :: rest -> String.get a 0
    | _ -> failwith "Napačen string"
  in
  (* Če razdelimo list glede na character, potem je dolžina lista enaka številu ponovitev characterja + 1*)
  let count_chars str char_1 = List.length (String.split_on_char char_1 str) - 1 in
  let valid_2 str char_1 index_list = match index_list with         (* Vrne ture, če je v podanem stringu na natanko enem izmed podanih indeksov char_1 *)
    | min :: max :: [] -> if String.get str (min - 1) = char_1 then String.get str (max - 1) != char_1 else String.get str (max - 1) = char_1
    |  _ -> failwith "Napačen index_list"
  in
  let int_of_bool bool_1 = if bool_1 then 1 else 0 in               (* Pretvorimo bool v int*)
  let to_lists str = match String.split_on_char ' ' str with        (* Vsako vrstico zapiše v seznam oblike [št. ponovitev znaka v kodi, 1_(vsebuje znak na natanko enem izmed indeksov), minimum ponovitev, max ponovitev]*)
    | a :: b :: c :: [] -> count_chars c (remove_colon b) :: int_of_bool (valid_2 c (remove_colon b) (limits_list a)) :: limits_list a
    | _ -> failwith "Napačen input"
  in 
  try
    while true; do 
      lines := to_lists (input_line ic) :: !lines    (*seznam = naslednja vrstica + seznam, vsaka vrstica pretvorjena v int*)
    done; !lines
  with End_of_file -> 
    close_in ic;
    List.rev !lines ;;


(* Ker smo tako packali v uvozu je naloga zdaj precej lahka; preverimo, ali je seštevek med min in max dovoljenim*)

let valid_pass_1 list_1 = match list_1 with
  | count :: x ::  min :: max :: [] -> if count >= min then count <= max else false
  | _ -> false

let valid_pass_2 list_1 = match list_1 with
  | count :: c :: min :: max :: [] -> if c = 1 then true else false
  | _ -> false

let rec count_valid_1 list_1 = match list_1 with
  | [] -> 0
  | x :: rest -> if valid_pass_1 x then 1 + count_valid_1 rest else count_valid_1 rest

let rec count_valid_2 list_1 = match list_1 with
  | [] -> 0
  | x :: rest -> if valid_pass_2 x then 1 + count_valid_2 rest else count_valid_2 rest

let solution_1 = count_valid_1 (read_file in_file);;

let solution_2 = count_valid_2 (read_file in_file);;




(* Export *)
let () =
  let print_output result out_file =
    let oc = open_out out_file in
    output_string oc result;
    close_out oc
  in 
  print_output (string_of_int solution_1) out_file_1;
  print_output (string_of_int solution_2) out_file_2;
