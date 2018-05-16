/* 185.A64 Compilers for Parallel Systems SS 2018 H.Moritsch
   EFL Parser
*/

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "efl_ir.h"

#define new(T) (T*) malloc(sizeof(T));

char *infile  = NULL;
char *outfile = "a.f90";

extern int yylineno;
extern char *yytext;
extern int yylex (void);

int yyerror(char *s) {
    printf("%s in line %d: %s\n", s, yylineno, yytext);
    }

extern void gen_decls(ENTRY ** sym);
extern void gen_stmts(N_STMTLIST * stmts);

ENTRY * lookup(ENTRY ** sym, char * id) {
    ENTRY * e;
    for (e = sym[FIRST]; e != NULL; e = e->next) {
        if (strcmp(id,e->id)==0) 
            return(e);
        }
    printf("{symbol %s not found}\n",id);
    return(NULL);
    }


%}

%union {
  char * id;
  float  fconst;
  int    iconst;
  int    enumcode;
  struct tARR_SIZE  * arraysize;
  struct tENTRY     * entry;
  struct tENTRY    ** entrylist;
  struct tN_PROG    * prog;
  struct tN_ASSIGN  * assign_stmt;
  struct tN_IF      * if_stmt;
  struct tN_FOR     * for_stmt;
  struct tN_STMT    * stmt;
  struct tN_STMTLIST* stmt_list;
  struct tN_EXPR    * expr;
  struct tN_EXPRLIST* expr_list;
  struct tN_VAR     * var;
}

%token PROGRAM END 
%token INT FLOAT
%token IF THEN ELSE ENDIF
%token FOR TO STEP ENDFOR 
%token ASSIGN EQ NE GT GE LT LE PLUS MINUS TIMES OVER AND OR NOT
%token LBR RBR LPAR RPAR COMMA COLON QUOT EOL

%token <id>     ID
%token <fconst> FCONST
%token <iconst> ICONST

%type <enumcode>    datatype relop addop multop unop

%type <prog>        program
%type <entry>       decl
%type <arraysize>   size

%type <assign_stmt> assign
%type <if_stmt>     if
%type <for_stmt>    for
%type <stmt>        stmt 
%type <stmt_list>   stmtlist else
%type <expr>        expr simpleexpr term factor step
%type <expr_list>   exprlist 
%type <var>         var

%start          program

%%

program:    PROGRAM ID EOL 
            declist
            stmtlist
            END EOL
                {
                prog->name   = $2;
                prog->stmts  = $5;
                }
            ;

declist:    declist decl
                {
                prog->symbols[LAST]->next = $2;
                prog->symbols[LAST] = $2;
                }
            | decl
                {
                prog->symbols[FIRST] = $1; 
                prog->symbols[LAST]  = $1;
                }
            ;

decl:       datatype ID EOL
                {
                $$ = new(ENTRY);
                $$->next = NULL;
                $$->id = $2;
                $$->data_type = $1;
                $$->dim_type  = _SCAL;
                $$->size = NULL;
                }
            | datatype ID LBR size RBR EOL
                {
                $$ = new(ENTRY);
                $$->next = NULL;
                $$->id = $2;
                $$->data_type = $1;
                $$->dim_type  = _ARR;
                $$->size = $4;
                }
            ;

datatype:   INT         { $$ = _INT;   }
            | FLOAT     { $$ = _FLOAT; }
            ;

size:       size COMMA ICONST COLON ICONST
                {
                $$ = $1;
                $$->rank ++;
                $$->lb[$$->rank] = $3;
                $$->ub[$$->rank] = $5;
                }
            | ICONST COLON ICONST
                {
                $$ = new(ARR_SIZE);
                $$->rank = 1;
                $$->lb[$$->rank] = $1;
                $$->ub[$$->rank] = $3;
                }
            ;

stmtlist:   stmtlist stmt  
                {
                $$ = $1;
                $$->last->next = $2;
                $$->last = $2;
                }
            | stmt
                {
                $$ = new(N_STMTLIST);
                $$->first = $1;
                $$->last  = $1;
                }
            ;

stmt:       assign
                {
                $$ = new(N_STMT);
                $$->next = NULL;
                $$->typ = _ASSIGN;
                $$->me.s_assign = $1;
                }
            | if
                {
                $$ = new(N_STMT);
                $$->next = NULL;
                $$->typ = _IF;
                $$->me.s_if = $1;
                }
            | for
                {
                $$ = new(N_STMT);
                $$->next = NULL;
                $$->typ = _FOR;
                $$->me.s_for = $1;
                }
            ;
            
assign:     var ASSIGN expr EOL
                {
                $$ = new(N_ASSIGN);
                $$->var_ref = $1;
                $$->expr = $3;
                }
            | QUOT expr EOL
                {
                $$ = new(N_ASSIGN);
                $$->var_ref = NULL;
                $$->expr = $2;
                }
            ;

if:         IF expr THEN EOL
            stmtlist
            else
            ENDIF EOL
                {
                $$ = new(N_IF);
                $$->expr = $2;
                $$->then_part = $5;
                $$->else_part = $6;
                }
            ;

else:       ELSE EOL
            stmtlist
                {
                $$ = $3;
                }
            |
                {
                $$ = NULL;
                }
            ;

for:        FOR ID ASSIGN expr TO expr step EOL
            stmtlist
            ENDFOR EOL
                {
                $$ = new(N_FOR);
                $$->loopvar = lookup(prog->symbols,$2);
                $$->lb = $4;
                $$->ub = $6;
                $$->step = $7;
                $$->body = $9;
                }
            ;

step:       STEP expr
                {
                $$ = $2;
                }
            |
                {
                $$ = NULL;
                }
            ;

exprlist:   exprlist COMMA expr  
                {
                $$ = $1;
                $$->last->next = $3;
                $$->last = $3;
                }
            | expr
                {
                $$ = new(N_EXPRLIST);
                $$->first = $1;
                $$->last  = $1;
                }
            ;

expr:       simpleexpr relop simpleexpr
                {
                $$ = new(N_EXPR);
                $$->next = NULL;
                $$->typ = _OP;
                $$->me.op.op1expr = $1;
                $$->me.op.oper = $2;
                $$->me.op.op2expr = $3;
                }
            | simpleexpr
                {
                $$ = $1;
                }
            ;

simpleexpr: simpleexpr addop term
                {
                $$ = new(N_EXPR);
                $$->next = NULL;
                $$->typ = _OP;
                $$->me.op.op1expr = $1;
                $$->me.op.oper = $2;
                $$->me.op.op2expr = $3;
                }
            | term
                {
                $$ = $1;
                }
            ;

term:       term multop factor
                {
                $$ = new(N_EXPR);
                $$->next = NULL;
                $$->typ = _OP;
                $$->me.op.op1expr = $1;
                $$->me.op.oper = $2;
                $$->me.op.op2expr = $3;
                }
            | factor
                {
                $$ = $1;
                }
            ;

factor:     unop factor 
                {
                $$ = new(N_EXPR);
                $$->next = NULL;
                $$->typ = _OP;
                $$->me.op.oper = $1;
                $$->me.op.op1expr = $2;
                }
            | LPAR expr RPAR
                {
                $$ = new(N_EXPR);
                $$->next = NULL;
                $$->typ = _OP;
                $$->me.op.oper = NO_OP;
                $$->me.op.op1expr = $2;
                }
            | var
                {
                $$ = new(N_EXPR);
                $$->next = NULL;
                $$->typ = _VAR;
                $$->me.var_ref = $1;
                }
            | FCONST
                {
                $$ = new(N_EXPR);
                $$->next = NULL;
                $$->typ = _FLOATNUM;
                $$->me.float_number = $1;
                }
            | ICONST
                {
                $$ = new(N_EXPR);
                $$->next = NULL;
                $$->typ = _INTNUM;
                $$->me.int_number = $1;
                }
            ;

var:        ID
                {
                $$ = new(N_VAR);
                $$->entry = lookup(prog->symbols,$1);
                $$->index = NULL;
                }
            | ID LBR exprlist RBR
                {
                $$ = new(N_VAR);
                $$->entry = lookup(prog->symbols,$1);
                $$->index = $3;
                }
            ;
            
relop:      EQ      { $$ = EQ_OP;}
            | NE    { $$ = NE_OP;}
            | GT    { $$ = GT_OP;}
            | GE    { $$ = GE_OP;}
            | LT    { $$ = LT_OP;}
            | LE    { $$ = LE_OP;}
            ;

addop:      PLUS    { $$ = PLUS_OP;}
            | MINUS { $$ = MINUS_OP;}
            | OR    { $$ = OR_OP;}
            ;

multop:     TIMES   { $$ = MULT_OP;}
            | OVER  { $$ = DIV_OP;}
            | AND   { $$ = AND_OP;}
            ;

unop:       MINUS   { $$ = MINUS_OP;}
            | NOT   { $$ = NOT_OP;}
            ;   

%%
void count_stmts(N_STMTLIST * stmts);

void count_if(N_IF * s) {
    count_stmts(s->then_part);
    if (s->else_part != NULL) {
        count_stmts(s->else_part);
        }
    }

void count_for(N_FOR * s) {
    count_stmts(s->body);
    }

void count_stmts(N_STMTLIST * stmts) {
    N_STMT * s;
    for (s = stmts->first; s != NULL; s = s->next) {
        switch(s->typ) {
            case _ASSIGN :  
                s->nr = ++stmt_count;
            break;
            case _IF:
                s->nr = ++stmt_count;
                count_if(s->me.s_if);   
            break;
            case _FOR:  
                s->nr = ++loop_count;
                count_for(s->me.s_for); 
            break;
            }
        }
    }
        
int main(int argc, char **argv) {

    prog = new(N_PROG);
    prog->symbols = (ENTRY **) malloc(2*sizeof(ENTRY));
    stmt_count = 0;
    loop_count = 900;
    if (yyparse()==0) {
        count_stmts(prog->stmts);
        printf("! Compilers for Parallel Systems\n");
        printf("! 185.A64 SS 2018 H. Moritsch\n");
        printf("! F90 generated  from EFL source\n");
        printf("\n");
        printf("program %s\n",prog->name);
        printf("\n");
        gen_decls(prog->symbols);
        printf("\n");
        gen_stmts(prog->stmts);
        printf("\n");
        printf("end program %s\n",prog->name);
        }
    }

