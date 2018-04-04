open Checker
open Printf
open Utils
open Grammar

let (ic, oc) = (open_in "input.txt", open_out "output.txt")

let rec print_assump list'=
    match list' with
    | []      -> ()
    | [x]     -> fprintf oc "%s" (string_of_expr x)
    | x :: xs -> begin
        fprintf oc "%s ," (string_of_expr x);
        print_assump xs
        end

let do_deduct =
    let (assump, b) = parse_assump (input_line ic) in
    let a           = List.hd assump in
    let worker      = new checker a oc in
    let pos         = ref 0 in
    worker#fill_assump assump 1;
    print_assump (List.rev (List.tl assump));
    fprintf oc "|-(%s->%s)\n" (string_of_expr a) (string_of_expr b);
    try
      while true; do
        let line = input_line ic in
        if (line <> "") then begin
            let expr    = parse_expr line in
            worker#check expr !pos;
            pos := !pos + 1
            end
      done;
    with End_of_file ->
      close_in ic;
      close_out oc;
