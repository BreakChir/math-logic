open Prover
open Printf
open Utils
open Grammar

let (ic, oc) = (open_in "input.txt", open_out "output.txt")

let rec dededuct assump e =
    match assump with
    | [] -> ()
    | x :: xs -> match e with
                | Binary (Impl, a, b) -> fprintf oc "%s\n%s\n" (string_of_expr a) (string_of_expr b);
                                         dededuct xs b
                | _ -> ()

let rec print_proof pr =
    match pr with
    | [] -> ()
    | x :: xs -> fprintf oc "%s\n" (string_of_expr x);
                 print_proof xs
                 
let rec print_assump list'=
    match list' with
    | []      -> ()
    | [x]     -> fprintf oc "%s" (string_of_expr x)
    | x :: xs -> begin
        fprintf oc "%s," (string_of_expr x);
        print_assump xs
        end                 

let do_prove =
    let header = input_line ic in
    let (assump, expr) = parse_head header in
    let pr = new prover assump expr oc in
    let (proof, e, ans) = pr#prove in
    if (ans = "") then begin 
                       print_assump (List.rev assump);
                       fprintf oc "|-%s\n" (string_of_expr expr);
                       print_proof proof;
                       dededuct assump e
                       end     
    else fprintf oc "%s" ans;
    close_in ic;
    close_out oc