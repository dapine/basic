open Syntax

type value = VInt of int
           | VString of string
           | VVar of string
           | VPrint of string

exception ExpUnrEval of string
exception ExpBinEval of string
exception ExpEval of string

let eval_int = function
    | ExpInt n -> n
    | _ -> 0

let eval_string = function
    | ExpStr s -> s
    | _ -> ""

let eval_expression = function
    | ExpInt n -> VInt n
    | ExpVar s -> VVar s
    | ExpStr s -> VString s
    | ExpUnr (op, e) -> (match op with
                            | UMINUS -> VInt (- (eval_int e))
                            | _ -> raise (ExpUnrEval "Could not evaluate a unary expression"))
    | ExpBin (e1, op, e2) -> (match op with
                            | PLUS -> VInt (eval_int e1 + eval_int e2)
                            | MINUS -> VInt (eval_int e1 - eval_int e2)
                            | MULT -> VInt (eval_int e1 * eval_int e2)
                            | DIV -> VInt (eval_int e1 / eval_int e2)
                            | MOD -> VInt (eval_int e1 mod eval_int e2)
                            | _ -> raise (ExpBinEval "Could not evaluate a binary expression"))

let string_of_eval = function
    | VInt n -> string_of_int n
    | VString str -> "\"" ^ str ^ "\""
    | VVar v -> v
    | VPrint p -> p

let eval_command = function
    | Print e -> VPrint (string_of_eval (eval_expression e))
    | _ -> VInt 0

let eval_line = function
    | {num=_; cmd=c} -> eval_command c
