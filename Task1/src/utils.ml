let (>>) x f = f x

let parse_str parser = (fun s -> s >> Lexing.from_string >> parser Lexer.main)
let parse_assump assump = parse_str Parser.assump assump
let parse_expr = parse_str Parser.main

let axioms = List.map (parse_str Parser.main)
            ["A->B->A";
             "(A->B)->(A->B->C)->(A->C)";
             "A->B->A&B";
             "A&B->A";  
             "A&B->B";
             "A->A|B";
             "B->A|B";
             "(A->C)->(B->C)->(A|B->C)";
             "(A->B)->(A->!B)->!A";
             "!!A->A"]

(* Doesn't use *)

let not_empty_pred = fun x -> x <> ""

let read_file filename =
 let lines = ref [] in
 let chan = open_in filename in
  try
    while true; do
      let l = input_line chan in
      lines := l :: !lines
    done; !lines
  with End_of_file ->
   close_in chan;
   List.rev !lines             