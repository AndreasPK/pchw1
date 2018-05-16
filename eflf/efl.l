/* 185.A64 Compilers for Parallel Systems SS 2018 H.Moritsch
   EFL Scanner
*/

%{
#include "efl.tab.h"
%}

%option yylineno

%%

program     return PROGRAM; 
end         return END; 

int         return INT;
float       return FLOAT;

if          return IF; 
then        return THEN; 
else        return ELSE; 
endif       return ENDIF; 

for         return FOR; 
to          return TO; 
step        return STEP; 
endfor      return ENDFOR; 

"="         return ASSIGN; 

"=="        return EQ; 
"<>"        return NE; 
">"         return GT; 
">="        return GE; 
"<"         return LT; 
"<="        return LE; 

"&&"        return AND; 
"||"        return OR; 
"!"         return NOT; 


"+"         return PLUS; 
"-"         return MINUS; 
"*"         return TIMES;
"/"         return OVER; 

"("         return LPAR; 
")"         return RPAR; 

"["         return LBR; 
"]"         return RBR; 

","         return COMMA; 
":"         return COLON;

"?"         return QUOT;

[a-zA-Z][a-zA-Z_0-9]*   yylval.id     = strdup(yytext); return ID;
[0-9]+"."[0-9]+         yylval.fconst = atof(yytext);   return FCONST;
[0-9]+                  yylval.iconst = atoi(yytext);   return ICONST;

^[ \t\r]*\n
[ \t\r]
\n          return EOL; 

^[ \t\r]*#.*\n
#.*

%%
