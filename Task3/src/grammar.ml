type binOp = Conj | Disj | Impl

let string_of_binop = function
    | Conj -> "&"
    | Disj -> "|"
    | Impl -> "->"

type expr = 
    | Binary of binOp * expr * expr
    | Not of expr
    | Var of string

let rec string_of_expr = function
    | Var s              -> s
    | Not e              -> "!" ^ string_of_expr e
    | Binary (op, a, b)  -> "(" ^ string_of_expr a ^ string_of_binop op ^ string_of_expr b ^ ")"

let (-->) a b = Binary (Impl, a, b)

type annotation = ModusPonens of int * int
    | Axiom of int
    | Assumption of int
    | NoProof
    
let string_of_ann = function
    | ModusPonens (a, b) -> "(M.P. " ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"
    | Axiom num          -> "(Сх. акс. " ^ string_of_int num ^ ")"
    | Assumption num     -> "(Предп. " ^ string_of_int num ^ ")"
    | NoProof            -> "(Не доказано)"