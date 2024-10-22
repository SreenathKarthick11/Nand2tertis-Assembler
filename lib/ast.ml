(* ast.ml *)
(* Modules used *)
open Symboltable
open Machine

type provar = string  (*program variable for labels and variable *)
type address = int (* address is of type int *)

(* the label is a record with name and address*)
type label = {name: provar;value: address}

(*jump inst in C instruction*)
type jump = NullJump | JGT | JEQ | JGE | JLT | JNE | JLE | JMP

(*destination inst in C instructions *)
type dest = NullDest | M | D | DM | A | AM | AD | ADM

(* computation inst in C instructions*)
type comp =
  | Zero | One | MinusOne | Dcomp | Acomp | NotDcomp | NotAcomp | MinusDcomp
  | MinusAcomp | DcompPlus1 | AcompPlus1 | DcompMinus1 | AcompMinus1
  | DPlusA | DMinusA | AMinusD | DAndA | DOrA | Mcomp | NotMcomp
  | MinusMcomp | McompPlus1 | McompMinus1 | DPlusM | DMinusM | MMinusD
  | DAndM | DOrM

(* type of instruction
label defintintions -Labeldef
C instructions      -Cinst
At instruction      -At
A instructions      -Ainst
 *)
type asminst = 
        Labeldef of provar
        | Cinst of dest * comp * jump
        | At of address
        | Ainst of provar

(*list of instruction*)
type program = asminst list

(* LABELS functions *)

(*checks if inst is a Label defintion*) 
let is_label (inst : asminst) : bool = 
        match inst with 
        Labeldef _label_name -> true
        | _ -> false

(*gets the label name  *)
let get_label_name (inst : asminst) : provar = 
        match inst with 
        Labeldef label_name -> label_name
        | _ -> ""

(* function to get labels from a list of inst *)
let rec get_labels_rec (prog :program) (current_line :address)=
        match prog with
        [] -> [] 
        | h :: t ->
                        if is_label h then
                                {name = get_label_name h; value = current_line} :: (get_labels_rec t current_line)
                        else
                                get_labels_rec t (current_line + 1)

let get_labels (prog : program) = get_labels_rec prog 0

(*check if 'provarible is in labels list*)

let rec is_in_labels (names :provar) (labels :(label list)) = 
        match labels with 
        [] -> false
        | h :: t ->
                        if h.name = names then true
                        else is_in_labels names t

(* gets the address with repect to the label from the record *)
let rec get_address_from_labelname (name:provar) (labels :(label list)) = 
        match labels with
        [] -> 0
        |h :: t ->
                        if h.name = name then h.value
                        else get_address_from_labelname name t

(* SYMBOLS - functions *)

(*Check if inst is ainst and not a label *) 
let is_at_var (inst :asminst) (labels :(label list)) = 
        match inst with 
        Ainst var_name -> if is_in_labels var_name labels then false
                          else true
        | _ -> false

(* gets name of variable of a ainst *)
let get_var_name (inst : asminst) : provar =
        match inst with
        Ainst var_name -> var_name
        | _ -> ""

(*function to add symbols to the symbol table from the module Symboltable *)
let rec symbol_table_rec (prog :program) (labels :(label list)) (table) (current_address : address) = 
        match prog with 
        [] -> ()
        |head :: tail ->
                        if is_at_var head labels then
                                let var_name = get_var_name head in
                                if not (SymbolTable.contains_symbol var_name) then
                                        let _unit_ = SymbolTable.add_symbol var_name current_address in 
                                        symbol_table_rec tail labels table (current_address + 1)
                                else
                                        symbol_table_rec tail labels table current_address
                        else
                                symbol_table_rec tail labels table current_address
   
let make_symboltable (prog :program) (labels :(label list)) table = symbol_table_rec prog labels table 16


(* TRANSLATION *)

(* A- instruction two types 
               -At inst : @address
               -Ainst inst : @variable *)

(* even function*) 
let even n = if (n mod 2 = 0) then true else false

(* odd function*)
let odd n = not (even n)

(* converting decimal to binary *)
let rec dec_to_bin number = 
        if number = 0 then 0
        else if (odd number) then dec_to_bin (number - 1) + 1
        else 10*(dec_to_bin(number/2))

let rec zfill string n = 
        if (String.length string >= n)
                then (String.sub string ((String.length string) - n) n)
        else 
                "0" ^ (zfill string (n-1))

(* gets the address of At inst*)
let get_address (inst :asminst) : address = 
        match inst with 
        At address -> address
        | _ -> 0

(* translate at inst *)
let translate_at (inst :asminst) : minst = 
        let add = get_address inst in
        let bin_add = zfill (string_of_int (dec_to_bin add )) 15 in 
        Minst ("0" ^ bin_add)

(* translate an 'Ainst' inst *)
let translate_ainst (inst : asminst) (labels :(label list)) _symb_table =
        let var_name = get_var_name inst in 
        if is_in_labels var_name labels then
                let label_address = get_address_from_labelname var_name labels in 
                        translate_at (At label_address)
        else
                let address = SymbolTable.get_address var_name in 
                        translate_at (At address)

(* C instruction (dest,comp,jump)*)

let jump_translate (inst :jump) = 
        match inst with 
        NullJump -> "000" | JGT -> "001" | JEQ -> "010" | JGE -> "011"
        | JLT -> "100" | JNE -> "101" | JLE -> "110" | JMP -> "111"

let dest_translate (inst : dest) = 
        match inst with 
        NullDest -> "000" | M -> "001" | D -> "010" | A -> "100"
        | DM -> "011" | AM -> "101" | AD -> "110" | ADM -> "111"

let comp_translate (inst : comp) = 
        match inst with 
        Zero -> "0101010" | One -> "0111111" | MinusOne -> "0111010" | Dcomp -> "0001100" | Acomp -> "0110000"
        | NotDcomp -> "0001101" | NotAcomp -> "0110001" | MinusDcomp -> "0001111" | MinusAcomp -> "0110011"
        | DcompPlus1 -> "0011111" | AcompPlus1 -> "0110111" | DcompMinus1 -> "0001110" | AcompMinus1 -> "0110010"
        | DPlusA -> "0000010"| DMinusA -> "0010011" | AMinusD -> "0000111" | DAndA -> "0000000" | DOrA -> "0010101"
        | Mcomp -> "1110000" | NotMcomp -> "1110001" | MinusMcomp -> "1110011" | McompPlus1 -> "1110111" | McompMinus1 -> "1110010"
        | DPlusM -> "1000010" | DMinusM -> "1010011" | MMinusD -> "1000111" | DAndM -> "1000000" | DOrM -> "1010101"


let translate_cinst (inst : asminst) =
        match inst with 
        Cinst (dest ,comp , jump) -> Minst ("111"^ (comp_translate comp) ^ (dest_translate dest) ^ (jump_translate jump))
        | _ -> Minst ("")


let rec translate_inst ( prog : program ) ( labels : ( label list )) symb_table =
       match prog with 
       [] -> []
       | h :: t -> 
                  let ti = translate_inst t labels symb_table in
                  (match h with 
                   Labeldef _name -> ti
                   | Cinst (dest,comp,jump) -> translate_cinst ( Cinst (dest,comp,jump)) :: ti
                   | At add -> translate_at (At add) :: ti
                   | Ainst name -> translate_ainst (Ainst name) labels symb_table :: ti)


(*final translate function on the list of instruction *)
let translate_program (prog : program) (table : (provar, address) Hashtbl.t) : minst list =
    let labels = get_labels prog in
    let symb_table = table in
    let () = make_symboltable prog labels symb_table in
    translate_inst prog labels symb_table


