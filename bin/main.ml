(* Your OCaml experiments and, eventually, your language's entry point. *)

type token = 
        | Ident of string
        | Int of int
        | Plus
        | Minus
        | Star
        | Slash
        | LParen
        | RParen
        | Equals
        | Semicolon

type expr = 
        | Number of int
        | Variable of string
        | Add of expr * expr
        | Subtract of expr * expr
        | Multiply of expr * expr
        | Division of expr * expr
        | Negate of expr

type stmt = 
        | Expression of expr
        | Assignment of string * expr

let _token_to_string token = 
        match token with
        | Int num -> string_of_int num
        | Ident ident -> ident
        | Equals -> "="
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
        | '=' -> Some Equals
        | '(' -> Some LParen
        | ')' -> Some RParen
        | ';' -> Some Semicolon
        | _ -> None

let is_digit character = 
        match character with
        | '0'..'9' -> true
        | _ -> false

let is_alpha character = 
  match character with
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false


let is_space character = 
        match character with
        | ' ' | '\n' | '\t' -> true
        | _ -> false


let rec digit_end source index = 
        if index >= String.length source then index
        else if is_digit source.[index] then digit_end source (index + 1)
        else index

let rec identifier_end source index = 
  if index >= String.length source then index
  else if is_alpha source.[index] || is_digit source.[index] then identifier_end source (index + 1)
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
                        else if is_alpha char then
                          let end_index = identifier_end source index in
                          let extracted = String.sub source index (end_index - index) in
                          scan (end_index) (Ident extracted :: acc)
                        else
                                match token_of_char source.[index] with
                                | Some token -> scan (index + 1) (token :: acc)
                                | None -> failwith (Printf.sprintf "Unexpected character %c at position %d" char index)
        in scan 0 []


let rec eval env expression = 
        match expression with
        | Number n -> n
        | Variable name -> (match List.assoc_opt name env with
                             | Some value -> value
                             | None -> failwith ("Unidentified identifer " ^ name)) 
        | Add (left, right) -> eval env left + eval env right
        | Subtract(left, right) -> eval env left - eval env right
        | Multiply(left, right) -> eval env left * eval env right
        | Division(left, right) -> eval env left / eval env right
        | Negate inner -> - (eval env inner)

let rec parse_primary tokens = 
        match tokens with
        | Int n :: rest -> (Number n, rest)
        | Ident name :: rest -> (Variable name, rest)
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
and parse_statement tokens = 
        match tokens with
        | Ident name :: Equals :: rest -> let (right, rest) = parse_expression rest in (Assignment(name, right), rest)
        | _ -> let (expr, rest) = parse_expression tokens in (Expression expr, rest)


let execute env statement = 
        match statement with
        | Expression expr -> let result = eval env expr in (result, env)
        | Assignment(name, expr) -> let result = eval env expr in (result, (name, result) :: env)


let run env source = 
        let tokens = lex_symbols source in 
        let (statement, rest) = parse_statement tokens in
        match rest with
        |[Semicolon] -> execute env statement
        | _ -> failwith "Expected a final Semicolon"

let rec prompt env = 
             print_string "Enter an expression: ";
             match read_line() with
             | exception End_of_file -> print_newline ()
             | "quit" -> ()
             | source ->
                 if String.trim source = "" then
                   prompt env
                 else begin
                 (try
                    let result, next_env = run env source in 
                    print_endline (string_of_int result);
                    prompt next_env
                  with 
                  |Failure message -> print_endline ("Error: " ^ message); prompt env 
                  |Division_by_zero -> print_endline "Error: division by zero"; prompt env)
                 end

let () = prompt []
