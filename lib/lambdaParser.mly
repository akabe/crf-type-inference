%{
open Lambda
%}

%token <string> IDENT
%token BACKSLASH
%token DOT
%token ELSE
%token FALSE
%token IF
%token LPAREN
%token RPAREN
%token SEMISEMI
%token THEN
%token TRUE

%start expr
%type <(string, unit) Lambda.t> expr
%%
expr:
  lam_expr SEMISEMI { $1 }
;

lam_expr:
  app_expr                                { $1 }
| BACKSLASH IDENT DOT lam_expr            { lam $2 $4 }
| IF lam_expr THEN lam_expr ELSE lam_expr { if_ $2 $4 $6 }
;

app_expr:
  simple_expr          { $1 }
| app_expr simple_expr { $1 $ $2 }
;

simple_expr:
  TRUE                   { tru }
| FALSE                  { fls }
| IDENT                  { var $1 }
| LPAREN lam_expr RPAREN { $2 }
;