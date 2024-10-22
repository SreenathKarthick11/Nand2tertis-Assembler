(* parser *)
(* libraries*)
 open Ast 

(* slice the input from start to end *)
let slice_str (input :string) (start: int) : string =
        if String.length input-1 < start then "" else
        String.sub input start ((String.length input) -start)

(* strip input of white spaces *)
let rec remove_whitespaces (input: string)=
        if (String.starts_with ~prefix:" " input)|| (String.starts_with ~prefix:"\t" input) then
                remove_whitespaces (slice_str input 1)
        else if (String.ends_with ~suffix:" " input)|| (String.ends_with ~suffix:"\t" input) then
                remove_whitespaces (String.sub input 0 ((String.length input)-1))
        else 
            input

(* removing comments *)
let rec remove_comment (input : string) : string =
        if input = "" then ""
        else if String.starts_with ~prefix:"//" input then ""
        else String.sub input 0 1 ^ (remove_comment (slice_str input 1))

let rec remove_empty_lines (input: (string list)) : (string list) =
        match input with
        [] -> []
        | h :: t -> let rel = remove_empty_lines t in
                        if h = "" then rel
                        else h::rel

(* split a given program into a list *)
let get_lines (program : string) : string list =
        let lines = String.split_on_char '\n' program in
        let lines_without_comments = List.map remove_comment lines in
        let stripped_lines = List.map remove_whitespaces lines_without_comments in
        remove_empty_lines stripped_lines

let alphabets ="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let nums = "0123456789"
let special_char = ":$,_."
let valid_provar_begin = special_char ^ alphabets
let valid_provar = valid_provar_begin ^ nums

let rec is_provar_post (input: string) : bool =
       if input = "" then true
       else String.contains valid_provar input.[0] && (is_provar_post (slice_str input 1))

let is_provar (input : string) : bool =
       if input="" then true
       else String.contains valid_provar_begin input.[0] && ( is_provar_post (slice_str input 1))

let rec is_digits (input : string) : bool =
       if input="" then true 
       else String.contains nums input.[0] && ( is_digits (slice_str input 1))

(* label validation *)       
let validate_Labeldef ( input : string ) : bool =
       if String.length input < 3 then false
       else ( input.[0] = '(' && input.[String.length input - 1] = ')' ) && 
            ( is_provar (String.sub input 1 ((String.length input) -2)))


(* At instruction validation *)
let validate_at (input : string) : bool =
        if String.length input < 2 then false
        else input.[0] = '@' && (is_digits (slice_str input 1))

(* Ainst instruction validation *)
let validate_ainst (input : string) : bool =
        if String.length input < 2 then false
        else input.[0] = '@' && (is_provar (slice_str input 1))

(* Cinst validation *)
let eqto_presence (input :string) : bool= if String.contains input '=' then true else false
let colon_presence (input : string ) : bool = if String.contains input ';' then true else false

let valid_jump = ["NullJump"; "JGT"; "JEQ"; "JGE"; "JLT"; "JNE"; "JLE"; "JMP"]
let is_valid_jump jump = List.mem jump valid_jump

let valid_dest = ["NullDest"; "M"; "D"; "DM";"MD";"AMD"; "A"; "AM"; "AD"; "ADM"]
let is_valid_dest dest = List.mem dest valid_dest

let valid_comp = [
  "0"; "1"; "-1"; "D"; "A"; "!D"; "!A"; "-D"; "-A";
  "D+1"; "A+1"; "D-1"; "A-1"; "D+A"; "D-A"; "A-D";
  "D&A"; "D|A"; "M"; "!M"; "-M"; "M+1"; "M-1"; "D+M";
  "D-M"; "M-D"; "D&M"; "D|M"]
let is_valid_comp comp = List.mem comp valid_comp


let validate_cinst (input : string) : bool =
  if eqto_presence input then
    match String.split_on_char '=' input with
    | [dest; comp_jump] -> 
        if colon_presence comp_jump then 
          match String.split_on_char ';' comp_jump with
          | [comp; jump] -> 
              let jumpbool = if jump = "" then true else is_valid_jump jump in
              (is_valid_dest dest) && (is_valid_comp comp) && jumpbool
          | _ -> false  
        else 
          let comp = comp_jump in
          (is_valid_dest dest) && (is_valid_comp comp)
    | _ -> false  
  else if colon_presence input then
    match String.split_on_char ';' input with
    | [comp; jump] -> 
        let jumpbool = if jump = "" then true else is_valid_jump jump in
        (is_valid_comp comp) && jumpbool
    | _ -> false  
  else 
    is_valid_comp input

(* Tokenization *)

let tokenize_Labeldef (input : string ) : asminst =
        Labeldef (String.sub input 1 ((String.length input ) -2))

let tokenize_at (input : string ) : asminst = 
        At (int_of_string (slice_str input 1))

let tokenize_ainst (input :string ) : asminst=
        Ainst (slice_str input 1) 

let tokenize_c_dest = function
  | "NullDest" -> NullDest | "M" -> M | "D" -> D | "DM" -> DM | "A" -> A | "AM" -> AM | "AD" -> AD | "ADM" -> ADM | "MD" -> DM | "AMD" -> ADM
  | _ -> NullDest

let tokenize_c_jump = function
  | "NullJump" -> NullJump | "JGT" -> JGT | "JEQ" -> JEQ | "JGE" -> JGE | "JLT" -> JLT
  | "JNE" -> JNE | "JLE" -> JLE | "JMP" -> JMP
  | _ -> NullJump

let tokenize_c_comp = function
  | "0" -> Zero | "1" -> One | "-1" -> MinusOne | "D" -> Dcomp | "A" -> Acomp | "!D" -> NotDcomp
  | "!A" -> NotAcomp | "-D" -> MinusDcomp | "-A" -> MinusAcomp | "D+1" -> DcompPlus1
  | "A+1" -> AcompPlus1 | "D-1" -> DcompMinus1 | "A-1" -> AcompMinus1 | "D+A" -> DPlusA
  | "D-A" -> DMinusA | "A-D" -> AMinusD | "D&A" -> DAndA | "D|A" -> DOrA
  | "M" -> Mcomp | "!M" -> NotMcomp | "-M" -> MinusMcomp | "M+1" -> McompPlus1
  | "M-1" -> McompMinus1 | "D+M" -> DPlusM | "D-M" -> DMinusM | "M-D" -> MMinusD | "D&M" -> DAndM | "D|M" -> DOrM
  | _ -> Zero

(*function looks complicated as to satisfy the matching exclusion *)
let tokenize_cinst (input : string) : asminst =
  if eqto_presence input then
    match String.split_on_char '=' input with
    | [dest; comp_jump] ->
        if colon_presence comp_jump then
          match String.split_on_char ';' comp_jump with
          | [comp; jump] ->
              Cinst (tokenize_c_dest dest, tokenize_c_comp comp, tokenize_c_jump jump)
          | _ -> 
              (* Handle the case where splitting fails *)
              Cinst (tokenize_c_dest dest, tokenize_c_comp comp_jump, tokenize_c_jump "NullJump")
        else 
          let comp = comp_jump in
          Cinst (tokenize_c_dest dest, tokenize_c_comp comp, tokenize_c_jump "NullJump")
    | _ -> 
        (* Handle unexpected case *)
        Cinst (tokenize_c_dest "NullDest", tokenize_c_comp input, tokenize_c_jump "NullJump")
  else if colon_presence input then
    match String.split_on_char ';' input with
    | [comp; jump] -> 
        Cinst (tokenize_c_dest "NullDest", tokenize_c_comp comp, tokenize_c_jump jump)
    | _ -> 
        (* Handle unexpected case *)
        Cinst (tokenize_c_dest "NullDest", tokenize_c_comp input, tokenize_c_jump "NullJump")
  else 
    Cinst (tokenize_c_dest "NullDest", tokenize_c_comp input, tokenize_c_jump "NullJump")

let tokenize_inst (input :string) : asminst =
    if validate_Labeldef input then tokenize_Labeldef input
    else if validate_at input then tokenize_at input
    else if validate_ainst input then tokenize_ainst input
    else if validate_cinst input then tokenize_cinst input
    else     failwith input

let tokenize_program (input :string) : (asminst list) = 
            let inst_list = get_lines input in
                List.map tokenize_inst inst_list
