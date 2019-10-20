open Basic.Syntax

let () =
    let lexbuf = Lexing.from_string "IF 10>9 THEN 10\nREM \"hello\"\nLET foo = 1+1"
    in let result = Basic.Parser.lines Basic.Lexer.token lexbuf
    in List.iter (fun a -> print_endline (pp_line a)) result
