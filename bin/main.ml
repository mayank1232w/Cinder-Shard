(* Your OCaml experiments and, eventually, your language's entry point. *)

type token = 
        | Int of int
        | Plus
        | Minus
        | Star
        | Slash
        | LParen
        | RParen
        | Semicolon


type expr = 
        | Number of int
        | Add of expr * expr
        | Subtract of expr * expr
        | Multiply of expr * expr
        | Division of expr * expr
        | Negate of expr

let _token_to_string token = 
        match token with
        | Int x -> string_of_int x
        | Plus -> "+"
        | Minus -> "-"
        | Star -> "*"
        | Slash -> "/"
        | LParen -> "("
        | RParen -> ")"
        | Semicolon -> ";"


let token_of_char character = 
        match character with
        | '+' -> Some Plus
        | '-' -> Some Minus
        | '*' -> Some Star
        | '/' -> Some Slash
        | '(' -> Some LParen
        | ')' -> Some RParen
        | ';' -> Some Semicolon
        | _ -> None

let is_digit character = 
        match character with
        | '0'..'9' -> true
        | _ -> false


let is_space character = 
        match character with
        | ' ' | '\n' | '\t' -> true
        | _ -> false


let rec digit_end source index = 
        if index >= String.length source then index
        else if is_digit source.[index] then digit_end source (index + 1)
        else index

let lex_symbols source = 
        let rec scan index acc = 
                if index >= String.length source then List.rev acc
                else
                        let char = source.[index] in
                        if is_space char then scan (index + 1) acc
                        else if is_digit char then
                                let end_index = digit_end source index in
                                let extracted = String.sub source index (end_index - index) in
                                let num = int_of_string extracted in scan (end_index) (Int num :: acc)
                        else
                                match token_of_char source.[index] with
                                | Some token -> scan (index + 1) (token :: acc)
                                | None -> failwith "Unexpected Character"
        in scan 0 []


let rec eval expression = 
        match expression with
        | Number n -> n
        | Add (left, right) -> eval left + eval right
        | Subtract(left, right) -> eval left - eval right
        | Multiply(left, right) -> eval left * eval right
        | Division(left, right) -> eval left / eval right
        | Negate inner -> - (eval inner)


let rec parse_primary tokens = 
        match tokens with
        | Int n :: rest -> (Number n, rest)
        | LParen :: rest -> let (expr, rest) = parse_expression rest in 
                                (match rest with
                                | RParen :: rest -> (expr, rest)
                                | _ -> failwith "Expected )")
        | _ -> failwith "Expected a number or ("
and parse_unary tokens = 
        match tokens with
        | Minus :: rest -> let (inner, rest) = parse_unary rest in (Negate inner, rest)
        | _ -> parse_primary tokens
and parse_term tokens = 
        let (left, remaining) = parse_unary tokens in
        let rec continue left tokens = 
                match tokens with
                | Star :: rest -> let (right, rest) = parse_unary rest in continue (Multiply(left, right)) rest
                | Slash :: rest -> let (right, rest) = parse_unary rest in continue (Division(left, right)) rest
                | _ -> (left, tokens)
        in continue left remaining
and parse_expression tokens = 
        let (first, remaining) = parse_term tokens in 
        let rec continue left tokens = 
                match tokens with
                | Plus :: rest -> let (right, rest) = parse_term rest in continue (Add (left, right)) rest
                | Minus :: rest -> let (right, rest) = parse_term rest in continue (Subtract (left, right)) rest
                | _ -> (left, tokens)
        in continue first remaining

let run source = 
        let tokens = lex_symbols source in 
        let (expression, rest) = parse_expression tokens in
        match rest with
        |[Semicolon] -> eval expression
        | _ -> failwith "Expected a final Semicolon"

let rec prompt () = 
             print_string "Enter an expression: ";
             let source = read_line() in
             if source = "quit" then () else
             let result = run source in 
             print_endline (string_of_int result);
             prompt()

let () = prompt()
