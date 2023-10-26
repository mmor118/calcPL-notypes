%{
open Ast
%}

%token <int> INT
%token PLUS
%token TIMES
%token LEQ
%token LPAREN
%token RPAREN
%token BOOL
%token LET
%token IF
%token THEN
%token ELSE
%token ERROR
%token <string> ID

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;

expr:
	| i = INT { Int i }
	| b = BOOL { Bool b }
	| id = ID { Var id }
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
	| e1 = expr; LEQ; e2 = expr { Binop (LEQ, e1, e2) }
	| LPAREN; e=expr; RPAREN { e }
	| LET; id = ID; "="; e1 = expr; "in"; e2 = expr { Let (id, e1, e2) }
	| IF; c = expr; THEN; t = expr; ELSE; f = expr { If (c, t, f) }
	| ERROR s { Error s }
	;
	
