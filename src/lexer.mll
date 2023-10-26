{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-' ? digit+
let identifier = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = 
  parse
  | white { read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "+" { PLUS }
  | "*" { TIMES }
  | "<=" { LEQ }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "true" { BOOL true }
  | "false" { BOOL false }
  | "let" { LET }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "error" { ERROR (Lexing.lexeme lexbuf) }
  | identifier as id { ID id }
  | eof { EOF }

