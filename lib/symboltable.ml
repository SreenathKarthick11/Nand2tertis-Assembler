(*symboltable.ml*)
(*Making a symboltable with Hashtable*)
module SymbolTable = struct
  let table = Hashtbl.create 200
  let predefined_symbols = [
    ("SP", 0); ("LCL", 1); ("ARG", 2); ("THIS", 3); ("THAT", 4);
    ("R0", 0); ("R1", 1); ("R2", 2); ("R3", 3); ("R4", 4);
    ("R5", 5); ("R6", 6); ("R7", 7); ("R8", 8); ("R9", 9);
    ("R10", 10); ("R11", 11); ("R12", 12); ("R13", 13);
    ("R14", 14); ("R15", 15); ("SCREEN", 16384); ("KBD", 24576)
  ]

  let init () =
    List.iter (fun (symbol, address) -> Hashtbl.add table symbol address) predefined_symbols

  (*add element to the symboltable*)
  let add_symbol symbol address =
    Hashtbl.add table symbol address

  (*check if elemet is an part of symboltable *)
  let contains_symbol symbol =
    Hashtbl.mem table symbol

  (*get address to corresponding to symbol in the symboltable*)
  let get_address symbol =
    Hashtbl.find table symbol
    
end

