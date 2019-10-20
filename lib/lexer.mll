{
    open Parser
    exception Error of string
    let error fmt = Printf.kprintf (fun msg -> raise (Error msg)) fmt
}

rule token = parse
    | [' ' '\t'] { token lexbuf }
    | ['\n'] { EOL }
    | eof { EOF }
    | ['0'-'9']+ as lxm { INT (int_of_string lxm) }
    | ['a'-'z']+ as lxm { VAR (lxm) }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { MULT }
    | '/' { DIV }
    | '%' { MOD }
    | '=' { EQUAL }
    | '<' { LESS }
    | "<=" { LESSEQ }
    | '>' { GREAT }
    | ">=" { GREATEQ }
    | "<>" { DIFF }
    | '&' { AND }
    | '|' { OR }
    | ';' { SEMICOLON }
    | "REM" { REM }
    | "GOTO" { GOTO }
    | "LET" { LET }
    | "PRINT" { PRINT }
    | "INPUT" { INPUT }
    | "IF" { IF }
    | "THEN" { THEN }
    | '"' { read_string (Buffer.create 17) lexbuf }

and read_string buf = parse
    | '"' { STRING (Buffer.contents buf) }
    | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
