%{
%}

%token <int> INT
%token <string> VAR
%token <string> STRING

%token PLUS MINUS MULT DIV MOD
%token EQUAL LESS LESSEQ GREAT GREATEQ DIFF
%token AND OR

%token SEMICOLON

%token REM GOTO LET PRINT INPUT IF THEN

%token EOL EOF

%left PLUS MINUS
%left TIMES DIV
%left MOD
%left EQUAL LESS LESSEQ GREAT GREATEQ DIFF
%nonassoc UMINUS NOT

%start lines

%type <Syntax.line list> lines

%%
expr:
    | INT { Syntax.ExpInt $1 }
    | VAR { Syntax.ExpVar $1 }
    | STRING { Syntax.ExpStr $1 }
    | MINUS expr %prec UMINUS { Syntax.ExpUnr (Syntax.UMINUS, $2) }
    | expr PLUS expr { Syntax.ExpBin ($1, Syntax.PLUS, $3) }
    | expr MINUS expr { Syntax.ExpBin ($1, Syntax.MINUS, $3) }
    | expr MULT expr { Syntax.ExpBin ($1, Syntax.MULT, $3) }
    | expr DIV expr { Syntax.ExpBin ($1, Syntax.DIV, $3) }
    | expr MOD expr { Syntax.ExpBin ($1, Syntax.MOD, $3) }
    | expr EQUAL expr { Syntax.ExpBin ($1, Syntax.EQUAL, $3) }
    | expr LESS expr { Syntax.ExpBin ($1, Syntax.LESS, $3) }
    | expr LESSEQ expr { Syntax.ExpBin ($1, Syntax.LESSEQ, $3) }
    | expr GREAT expr { Syntax.ExpBin ($1, Syntax.GREAT, $3) }
    | expr GREATEQ expr { Syntax.ExpBin ($1, Syntax.GREATEQ, $3) }
    | expr DIFF expr { Syntax.ExpBin ($1, Syntax.DIFF, $3) }
    | expr AND expr { Syntax.ExpBin ($1, Syntax.AND, $3) }
    | expr OR expr { Syntax.ExpBin ($1, Syntax.OR, $3) }
;

cmd:
    | REM STRING { Syntax.Rem $2 }
    | GOTO INT { Syntax.Goto $2 }
    | LET VAR EQUAL expr { Syntax.Let ($2, $4) }
    | INPUT VAR { Syntax.Input $2 }
    | IF expr THEN INT { Syntax.If ($2, $4) }
    | PRINT expr { Syntax.Print $2 }
;

line: cmd { let ln = Parsing.symbol_start_pos () in {num=ln.pos_lnum; cmd=$1} : Syntax.line }
;

lines: lines_ EOF { List.rev $1 }
;

lines_ :
    | line { [$1] }
    | lines_ EOL line { $3 :: $1 }
;
