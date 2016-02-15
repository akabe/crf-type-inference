type token =
  | IDENT of (string)
  | BACKSLASH
  | DOT
  | ELSE
  | FALSE
  | IF
  | LPAREN
  | RPAREN
  | SEMISEMI
  | THEN
  | TRUE

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (string, unit) Lambda.t
