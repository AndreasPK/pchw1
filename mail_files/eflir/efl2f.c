/* 185.A64 Compilers for Parallel Systems SS 2016 H.Moritsch
   EFL to Fortran 90
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "efl_ir.h"

int idepth  = 0;

void indent(int d) {
    int i;
    for (i=0; i<4*d; i++) 
        printf(" ");
    }

void gen_decls(ENTRY ** sym) {
    ENTRY * e;
    int i;
    for (e = sym[FIRST]; e != NULL; e = e->next) {
        switch (e->data_type) {
            case _INT:      printf("integer");  break;
            case _FLOAT:    printf("real");     break;
            }
        if (e->dim_type == _ARR) {
            printf(", dimension(");
            for (i=1; i<= e->size->rank; i++) {
                if (i>1) printf(",");
                printf("%d:%d",e->size->lb[i],e->size->ub[i]);
                }
            printf(")");
            }
        printf(" :: %s\n",e->id);
        }
    }

void gen_expr(N_EXPR * ex);

void gen_exprlist(N_EXPRLIST * exl) {
    N_EXPR * ex;
    for (ex = exl->first; ex != NULL; ex = ex->next) {
        if (ex != exl->first) printf(",");
        gen_expr(ex);
        }
    }

void gen_var_ref(N_VAR * v, BOOL write) {
    ENTRY * e;
    e = v->entry;
    printf("%s",e->id);
    if (e->dim_type == _SCAL) {
        if (v->index != NULL) 
            printf("{no index expression allowed}\n");
        }
    else {
        if (v->index == NULL) 
            printf("{index expression list missing}\n");
        printf("(");
        gen_exprlist(v->index);
        printf(")");
        }
    }

void gen_oper(int o) {
  switch(o) {
  case EQ_OP:       printf("==");    break;
  case NE_OP:       printf("<>");   break;
  case GT_OP:       printf(">");    break;
  case GE_OP:       printf(">=");   break;
  case LT_OP:       printf("<");    break;
  case LE_OP:       printf("<=");   break;
  case PLUS_OP:     printf("+");    break;
  case MINUS_OP:    printf("-");    break;
  case MULT_OP:     printf("*");    break;
  case DIV_OP:      printf("/");    break;
  case AND_OP:      printf(".and."); break;
  case OR_OP:       printf(".or.");  break;
  case NOT_OP:      printf(".not."); break;
  }
}

void gen_expr(N_EXPR * ex) {
    switch(ex->typ) {
        case _FLOATNUM:
            printf("%f",ex->me.float_number);
        break;
        case _INTNUM:
            printf("%d",ex->me.int_number);
        break;
        case _VAR:
            gen_var_ref(ex->me.var_ref,FALSE);
        break;
        case _OP:
            if (ex->me.op.oper == NO_OP) {
                printf("(");
                gen_expr(ex->me.op.op1expr);
                printf(")");
                }
            else if (ex->me.op.op2expr == NULL) {
                gen_oper(ex->me.op.oper);
                gen_expr(ex->me.op.op1expr);
                }
            else {
                gen_expr(ex->me.op.op1expr);
                gen_oper(ex->me.op.oper);
                gen_expr(ex->me.op.op2expr);
                }
        }
    }

void gen_assign(N_ASSIGN * s, int nr) {

    ENTRY * e;
    if (s->var_ref != NULL) 
        e = s->var_ref->entry;
        if (e->dim_type == _ARR) {
            /* 
            indent(idepth);
            FILL IN
            log write access to array element:
            printf("    write(*,*) <statement label, access ("write"), array name, indices> \n");
            */
            }
    printf("%03d ",nr);
    indent(idepth);
    if (s->var_ref != NULL) {
        gen_var_ref(s->var_ref,TRUE);
        printf(" = ");
        }
    else {
        printf("write(*,*) ");
        }
    gen_expr(s->expr);
    printf("\n");
    }

void gen_stmts(N_STMTLIST * stmts);

void gen_if(N_IF * s, int nr) {
    printf("%03d ",nr);
    indent(idepth);
    printf("if (");
    gen_expr(s->expr);
    printf(") then\n");
    idepth++;
    gen_stmts(s->then_part);
    if (s->else_part != NULL) {
        indent(--idepth);
        printf("    else\n");
        idepth++;
        gen_stmts(s->else_part);
        }
    indent(--idepth);
    printf("    end if");
    printf("\n");
    }

void gen_for(N_FOR * s, int nr) {
    printf("%03d ",nr);
    indent(idepth);
    printf("do %s = ",s->loopvar->id);
    gen_expr(s->lb);
    printf(", ");
    gen_expr(s->ub);
    if (s->step!=NULL) {
        printf(", ");
        gen_expr(s->step);
        }
    printf("\n");
    idepth++;
    gen_stmts(s->body);
    indent(--idepth);
    printf("    end do\n");
    }

void gen_stmts(N_STMTLIST * stmts) {
    N_STMT * s;
    for (s = stmts->first; s != NULL; s = s->next) {
        switch(s->typ) {
            case _ASSIGN:   
                gen_assign(s->me.s_assign,s->nr);
            break;
            case _IF:       
                gen_if(s->me.s_if,s->nr);
            break;
            case _FOR:      
                gen_for(s->me.s_for,s->nr);
            break;
            }
        }
    }

