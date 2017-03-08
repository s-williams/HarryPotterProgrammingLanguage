/* File parser.mly */
%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token FUNCTION LBRACE RETURN RBRACE EQUAL ORDER PRINT ERROR FOR
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start parser_main             /* the entry point */
%type <int> parser_main
%%
parser_main:
   expr EOL                { $1 }
;
expr:
   INT                     { $1 }
 | LPAREN expr RPAREN      { $2 }
 | expr PLUS expr          { $1 + $3 }
 | expr MINUS expr         { $1 - $3 }
 | expr TIMES expr         { $1 * $3 }
 | expr DIV expr           { $1 / $3 }
 | MINUS expr %prec UMINUS { - $2 }
;

