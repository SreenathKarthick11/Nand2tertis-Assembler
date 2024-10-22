(* main.ml*)
(* Libraries called *)
open Nand2tetris.Parser
open Nand2tetris.Ast
open Nand2tetris.Machine
open Nand2tetris.Symboltable 


let _ = SymbolTable.init () 

let f () =                (* f is a read line function to check have we reached end of file *)
try read_line () with
| End_of_file -> "\026"

let rec read_till_eof () =                  (* the function to read till the end of file *)
        let input = f () in
        if input = "\026" then ""
        else input ^ "\n" ^ (read_till_eof ())

let ()  = let input = read_till_eof () in
        let parsed_output = tokenize_program input in (* parsing the input from parser.ml *)
        let translated_output = translate_program parsed_output SymbolTable.table in  (* translating with the help of ast.ml *)
        let outputlist = translate_minst_program translated_output in  (* translating machine inst with the help of machine.ml *)
        List.iter (fun output ->
                let string_output = output in
                print_endline string_output; (* printing the output *)
                ) outputlist


