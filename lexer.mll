(* File lexer.mll *)
{
open Parser        (* The type lexer_main is defined in parser.mli *)
exception Eof
}
rule lexer_main = parse
      [' ' '\t']     { lexer_main lexbuf }     (* skip blanks *)
    | ['\n' ]  { EOL }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | "accio"			{ FUNCTION }
    | "alohomora"       { LBRACE }
	| "avadakadavra"	{ RETURN }
	| "colloportus"		{ RBRACE }
	| "petrificus"		{ EQUAL }
	| "repairo"			{ ORDER }
	| "sonorus"			{ PRINT }
	| "sectumsepris"	{ ERROR }
	| "leviosa"			{ FOR }
    | '*'      { TIMES }
    | '/'      { DIV }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | eof      { raise Eof }
