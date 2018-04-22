let (>>) x f = f x

let parse_str parser = (fun s -> s >> Lexing.from_string >> parser Lexer.main)
let parse_head head = parse_str Parser.main head
let parse_expr a b = (fun expr -> expr >> Lexing.from_string >> Parser.expr (Lexer.expr a b))
let parse_ex = parse_str Parser.ax

let not_empty_pred = fun x -> x <> ""

let read_file filename =
 let lines = ref [] in
 let chan = open_in filename in
  try
    while true do
      let l = input_line chan in
      lines := l :: !lines
    done;
    !lines
  with End_of_file ->
   close_in chan;
   List.rev !lines             