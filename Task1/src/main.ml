open Checker
open Printf
open Utils
open Grammar

let do_check =
    let (ic, oc) = (open_in "input.txt", open_out "output.txt") in
    let assump = parse_assump (input_line ic) in
    let pos = ref 0 in
    checker#fill_assump assump 1;
    try
      while true; do
        let line = input_line ic in
        if (line <> "") then begin
            let expr = parse_expr line in
            let ann  = checker#check expr !pos in
            fprintf oc "(%d) %s %s\n" !pos (string_of_expr expr) (string_of_ann ann);
            pos := !pos + 1
            end
      done;
    with End_of_file ->
      close_in ic;
      close_out oc;
