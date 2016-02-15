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

open Parsing;;
let _ = parse_error;;
# 2 "lambdaParser.mly"
open Lambda
# 19 "lambdaParser.ml"
let yytransl_const = [|
  258 (* BACKSLASH *);
  259 (* DOT *);
  260 (* ELSE *);
  261 (* FALSE *);
  262 (* IF *);
  263 (* LPAREN *);
  264 (* RPAREN *);
  265 (* SEMISEMI *);
  266 (* THEN *);
  267 (* TRUE *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\004\000\004\000\
\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\004\000\006\000\001\000\002\000\001\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\009\000\000\000\008\000\000\000\000\000\007\000\
\011\000\000\000\000\000\005\000\000\000\000\000\000\000\001\000\
\006\000\000\000\000\000\010\000\003\000\000\000\000\000\004\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\012\000"

let yysindex = "\001\000\
\003\255\000\000\000\000\002\255\000\000\003\255\003\255\000\000\
\000\000\253\254\014\255\000\000\004\255\001\255\008\255\000\000\
\000\000\003\255\003\255\000\000\000\000\016\255\003\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\007\000"

let yytablesize = 28
let yytable = "\014\000\
\015\000\001\000\013\000\003\000\004\000\016\000\018\000\005\000\
\006\000\007\000\019\000\021\000\022\000\008\000\003\000\020\000\
\024\000\017\000\005\000\023\000\007\000\002\000\000\000\000\000\
\008\000\002\000\002\000\002\000"

let yycheck = "\006\000\
\007\000\001\000\001\001\001\001\002\001\009\001\003\001\005\001\
\006\001\007\001\010\001\018\000\019\000\011\001\001\001\008\001\
\023\000\011\000\005\001\004\001\007\001\004\001\255\255\255\255\
\011\001\008\001\009\001\010\001"

let yynames_const = "\
  BACKSLASH\000\
  DOT\000\
  ELSE\000\
  FALSE\000\
  IF\000\
  LPAREN\000\
  RPAREN\000\
  SEMISEMI\000\
  THEN\000\
  TRUE\000\
  "

let yynames_block = "\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lam_expr) in
    Obj.repr(
# 21 "lambdaParser.mly"
                    ( _1 )
# 103 "lambdaParser.ml"
               : (string, unit) Lambda.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 25 "lambdaParser.mly"
                                          ( _1 )
# 110 "lambdaParser.ml"
               : 'lam_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'lam_expr) in
    Obj.repr(
# 26 "lambdaParser.mly"
                                          ( lam _2 _4 )
# 118 "lambdaParser.ml"
               : 'lam_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'lam_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'lam_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'lam_expr) in
    Obj.repr(
# 27 "lambdaParser.mly"
                                          ( if_ _2 _4 _6 )
# 127 "lambdaParser.ml"
               : 'lam_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 31 "lambdaParser.mly"
                       ( _1 )
# 134 "lambdaParser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 32 "lambdaParser.mly"
                       ( _1 $ _2 )
# 142 "lambdaParser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "lambdaParser.mly"
                         ( tru )
# 148 "lambdaParser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "lambdaParser.mly"
                         ( fls )
# 154 "lambdaParser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "lambdaParser.mly"
                         ( var _1 )
# 161 "lambdaParser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lam_expr) in
    Obj.repr(
# 39 "lambdaParser.mly"
                         ( _2 )
# 168 "lambdaParser.ml"
               : 'simple_expr))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (string, unit) Lambda.t)
