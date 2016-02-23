{
open LambdaParser

let keyword_table = Hashtbl.create 5
let () = List.iter
    (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "if", IF;
      "then", THEN;
      "else", ELSE;
      "true", TRUE;
      "false", FALSE; ]

let comment_nest = ref 0
}

let identifier = ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_' '\'']*

rule token = parse
  [' ' '\t' ]  { token lexbuf } (* skip blanks *)
| ['\n' '\r' ] { Lexing.new_line lexbuf ; token lexbuf }
| "(*"         { incr comment_nest ; comment lexbuf }
| ";;"         { SEMISEMI }
| '\\'         { BACKSLASH }
| '.'          { DOT }
| '('          { LPAREN }
| ')'          { RPAREN }
| identifier   { let s = Lexing.lexeme lexbuf in
                 try Hashtbl.find keyword_table s
                 with Not_found -> IDENT s }
| _            { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }
| eof          { raise End_of_file }

and comment = parse
  "(*" { incr comment_nest ; comment lexbuf }
| "*)" { decr comment_nest ; if !comment_nest = 0 then token lexbuf else comment lexbuf }
| eof  { raise End_of_file }
| _    { comment lexbuf }