%{
  open Grammar;;
%}
%token <string> VAR
%token <Grammar.expr> EXPR
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
%start expr
%start ax
%type <Grammar.expr list * Grammar.expr> main
%type <Grammar.expr> expr
%type <Grammar.expr> ax
%%
ax:
        exp EOF          { $1 }
main:
        DEQ exp EOF   { ([], $2) }
        |ex DEQ exp EOF   { ($1, $3) }
exp:
        VAR              { Var ($1) }
        |EXPR            { $1 }
        |OPEN exp CLOSE  { $2 }     
        |NOT exp         { Not ($2) }  
        |exp IMPL exp    { Binary (Impl, $1, $3) }
        |exp AND exp     { Binary (Conj, $1, $3) }
        |exp OR exp      { Binary (Disj, $1, $3) }
ex:
        exp              { [ $1 ] }
        |ex COMMA exp    { $3 :: $1 }
        
expr:
        exp EOF          { $1 }
