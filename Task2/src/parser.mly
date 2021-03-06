%{
  open Grammar;;
%}
%token <string> VAR
%token IMPL AND OR NOT
%token OPEN CLOSE
%token EOF
%token DEQ COMMA
%right IMPL
%left DEQ
%left COMMA
%left OR
%left AND
%nonassoc NOT
%start main
%start assump
%type <Grammar.expr> main
%type <Grammar.expr list * Grammar.expr> assump
%%
main:
        exp EOF          { $1 }
exp:
        VAR              { Var ($1) }            
        |OPEN exp CLOSE  { $2 }     
        |NOT exp         { Not ($2) }  
        |exp IMPL exp    { Binary (Impl, $1, $3) }
        |exp AND exp     { Binary (Conj, $1, $3) }
        |exp OR exp      { Binary (Disj, $1, $3) }

        
assump:
        ex DEQ exp EOF   { ($1, $3) }
ex:
        exp              { [ $1 ] }
        |ex COMMA exp    { $3 :: $1 }
        