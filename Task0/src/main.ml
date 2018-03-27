open Grammar;;
open Buffer;;
open Printf;;

let (>>) x f = f x;;

let (ic,oc) = (open_in "input.txt", open_out "output.txt");;

ic >> input_line >> Lexing.from_string >> Parser.main Lexer.main >> string_of_expr >> fprintf oc "%s\n";;

close_out oc;;
close_in ic;;
