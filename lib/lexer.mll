{
open Core
open Parser
}

let lower = ['a' - 'z']
let digit = ['0' - '9']

let space = [' ' '\t']
let newline = '\r'? '\n'

let ident = lower (lower | digit)*
let var = ident
let label = "#" ident

let sign = '-'?
let int = sign digit+

rule read = 
  parse
  (* keywords *)
  | "if"          { IF }
  | "then"        { THEN }
  | "goto"        { GOTO }
  | "ret"         { RET }

  (* reserved operators *)
  | ":="          { ASSIGN }
  | "&"           { ADDRESS_OF }
  | "+"           { PLUS }
  | "-"           { SUB }
  | "*"           { MUL }
  | ">"           { GREATER_THAN }
  | "="           { EQUAL }
  | "<"           { LESS_THAN }
  | "!="          { NOT_EQUAL }
  | ";"           { SEMI_COLON }
  | ":"           { COLON }
  | ","           { COMMA }

  (* variables *)
  | label         { LABEL (Lexing.lexeme lexbuf)}
  | var           { VAR (Lexing.lexeme lexbuf) }

  (* literals *)
  | int           { INT (Int.of_string (Lexing.lexeme lexbuf)) } 

  (* whitespace *)
  | space+        { read lexbuf }
  | newline       { read lexbuf }

  (* braces *)
  | "("           { LEFT_PAREN }
  | ")"           { RIGHT_PAREN }
  | "["           { LEFT_BRACKET }
  | "]"           { RIGHT_BRACKET }

  | eof           { EOF }
  | _             { failwith ("Lexer error: " ^ Lexing.lexeme lexbuf) }