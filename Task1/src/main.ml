open Checker
open Printf
open Utils
open Grammar

let lines = read_file "input.txt"
let assumpS = List.hd lines 
let exprsS = List.filter emptyPred (List.tl lines)
let assump = parseAssump assumpS 
let exprs = List.map (parseExpr) exprsS
let anns = checkProof exprs assump
let arrExpr = Array.of_list exprs
let oc = open_out "output.txt"

let _ = 
    for i = 0 to Array.length arrExpr - 1 do
        fprintf oc "%s\n" ("(" ^ string_of_int (i + 1) ^ ") " 
                ^ string_of_expr arrExpr.(i) ^ " "^ string_of_ann anns.(i))
    done;
    close_out oc



