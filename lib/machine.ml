 (* converting binary encoding of asminst to minst ,that is binary to bytes*)
(*
type minst = Minst of string  (* machine instruction*)
type minst_program = minst list (*list of machine instruction *)

type bin = Bytes.t  (* a type for binary output *)
type bin_program = bin list (* list of bytes*)

(* to get the binary str *)
let get_minst_str = function
        | Minst str -> str

(* convert a binary to decimal *)
let rec bin_to_decimal n =
        if n = 0 then 0
        else if (n mod 10 = 0) then 2 * (bin_to_decimal (n/10))
        else bin_to_decimal (n-1)+1

(* to get the first and second part of 16 bit binary that is 8 bit each*)
let first_8_bits (inst: minst) = String.sub (get_minst_str inst) 0 8
let second_8_bits (inst: minst)= String.sub (get_minst_str inst) 8 8

(* convert 8 bit string to char*)
let bytestr_to_char str=
        char_of_int (bin_to_decimal(int_of_string str))

(* converting a minst to corresponding output*)
let minst_to_bin (inst: minst) : bin =
        let output = Bytes.create 2 in 
        let _unit1 = Bytes.set output 0 (bytestr_to_char (first_8_bits inst)) in
        let _unit2 = Bytes.set output 1 (bytestr_to_char (second_8_bits inst)) in
        output

(* converting a minst program *)
let translate_minst_program (program : minst_program):bin_program = List.map minst_to_bin program
*)


(* the above code was used to transform to bytes but now we have been asked to have the code in binary *)

(* converting to binary strings *)
type minst = Minst of string  (* machine instruction*)
type minst_program = minst list (*list of machine instruction *)

let get_minst_str = function
        | Minst str -> str

let translate_minst_program (program : minst_program ) : string list = List.map get_minst_str program 
