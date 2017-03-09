(* File lexer.mll *)
{
open Parser        (* The type lexer_main is defined in parser.mli *)
exception Eof
}
rule lexer_main = parse
      [' ' '\t']     { lexer_main lexbuf }     (* skip blanks *)
    | ['\n' ]  { EOL }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | "alohomora"       { LBRACE }
	| "avadakadavra"	{ RETURN }
	| "colloportus"		{ RBRACE }
	| "petrificus"		{ EQUAL }
	| "repairo"			{ ORDER }
	| "sectumsepris"	{ ERROR }
	| "leviosa"			{ FOR }
	| "sonorus"			{ PRINTLIST }
	| "reducto"			{ GETLISTOFSIZE }
	| "intersectify"	{ INTERSECT }
	| "unionify"		{ UNION }
	| "scourgify"	{ REMOVEDUPLICATES }
	| "diffindo"		{ GETLISTOFSIZE }
	| "accio"			{ SORTALPHABETICALLY}
	| "gemino"			{ APPEND }
    | '*'      { TIMES }
    | '/'      { DIV }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | eof      { raise Eof }
