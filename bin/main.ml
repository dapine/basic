(* open Basic.Syntax *)

(* let () = *)
(*     let lexbuf = Lexing.from_string "IF 10>9 THEN 10\nREM \"hello\"\nLET foo = 1+1" *)
(*     in let result = Basic.Parser.lines Basic.Lexer.token lexbuf *)
(*     in List.iter (fun a -> print_endline (pp_line a)) result *)

let () =
    let lexbuf = Lexing.from_string "PRINT 1+1\nPRINT 2-2\nPRINT 99 * 2\nPRINT 10/2\nPRINT 3%2"
    in let result = Basic.Parser.lines Basic.Lexer.token lexbuf
    in let evaluated = List.map (fun a -> Basic.Eval.eval_line a) result
    in List.iter (fun a -> print_endline (Basic.Eval.string_of_eval a)) evaluated
