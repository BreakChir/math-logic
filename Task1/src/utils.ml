let (>>) x f = f x

let parseStr parser = (fun s -> s >> Lexing.from_string >> parser Lexer.main)
let parseAssump assump = parseStr Parser.assump assump
let parseExpr = parseStr Parser.main
let emptyPred = fun x -> x <> ""

let axioms = List.map (parseStr Parser.main)
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