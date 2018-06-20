/* 185.A64 Compilers for Parallel Systems SS 2016 H.Moritsch
   EFL to Fortran 90
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "efl_ir.h"

int itdepth  = 0;

void tindent(int d) {
    int i;
    // for (i=0; i<4*d; i++) 
    //    printf(" ");
    for (i=0; i<d; i++) 
    	printf("\t");
    }

void ir_decls(ENTRY ** sym) {
    ENTRY * e;
    int i;
//    printf("SYMBOLS\n");
    for (e = sym[FIRST]; e != NULL; e = e->next) {
        printf("%s\t",e->id);
        switch (e->data_type) {
            case _INT:      printf("INT");  break;
            case _FLOAT:    printf("FLOAT");     break;
            }
        if (e->dim_type == _ARR) {
            printf("\t%d", e->size->rank);
            for (i=1; i<= e->size->rank; i++) {
                 printf("\t%d\t%d",e->size->lb[i],e->size->ub[i]);
                }
            }
		printf("\n");
        }
//    printf("/SYMBOLS\n");
    }

void ir_expr(N_EXPR * ex);

void ir_exprlist(N_EXPRLIST * exl) {
    N_EXPR * ex;
	tindent(itdepth);
    printf("EXPRLIST\n");
	itdepth++;
    for (ex = exl->first; ex != NULL; ex = ex->next) {
        ir_expr(ex);
        }
	tindent(--itdepth);
    printf("/EXPRLIST\n");
    }

void ir_var_ref(N_VAR * v, BOOL write) {
    ENTRY * e;
    e = v->entry;
	tindent(itdepth);
    printf("VAR\n");
	tindent(++itdepth);
    printf("ENTRY %s\n",e->id);
    if (e->dim_type == _SCAL) {
        if (v->index != NULL) 
            printf("{no index expression allowed}\n");
        }
    else {
        if (v->index == NULL) 
            printf("{index expression list missing}\n");
        ir_exprlist(v->index);
        }
	itdepth--;    
	}

void ir_oper(int o) {
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
  printf("\n");
}

void ir_expr(N_EXPR * ex) {
	tindent(itdepth);
    printf("EXPR\n");
    switch(ex->typ) {
        case _FLOATNUM:
			tindent(itdepth);
            printf("FLOAT %f\n",ex->me.float_number);
        break;
        case _INTNUM:
			tindent(itdepth);
            printf("INT %d\n",ex->me.int_number);
        break;
        case _VAR:
            ir_var_ref(ex->me.var_ref,FALSE);
        break;
        case _OP:
            if (ex->me.op.oper == NO_OP) {
//				tindent(itdepth);
//              printf("(\n");
				itdepth++;
                ir_expr(ex->me.op.op1expr);
				itdepth--;
//				tindent(itdepth);
//              printf(")\n");
                }
            else if (ex->me.op.op2expr == NULL) {
				tindent(itdepth);
				printf("UNOP ");
                ir_oper(ex->me.op.oper);
				itdepth++;
                ir_expr(ex->me.op.op1expr);
				itdepth--;
                }
            else {
				tindent(itdepth);
				printf("BINOP ");
				ir_oper(ex->me.op.oper);
				itdepth++;
                ir_expr(ex->me.op.op1expr);
                ir_expr(ex->me.op.op2expr);
				itdepth--;
                }
        }
    }

void ir_assign(N_ASSIGN * s, int nr) {
    ENTRY * e;
    if (s->var_ref != NULL) 
        e = s->var_ref->entry;
        if (e->dim_type == _ARR) {
            /* 
            tindent(itdepth);
            FILL IN
            log write access to array element:
            printf("    write(*,*) <statement label, access ("write"), array name, indices> \n");
            */
            }
    tindent(itdepth);
	printf("ASSIGN @%03d\n",nr);
	itdepth++;
    if (s->var_ref != NULL) {
        ir_var_ref(s->var_ref,TRUE);
        }
    ir_expr(s->expr);
	itdepth--;
    }

void ir_stmts(N_STMTLIST * stmts);

void ir_if(N_IF * s, int nr) {
    tindent(itdepth);
    printf("IF @%03d\n",nr);
	itdepth++;
    ir_expr(s->expr);
    tindent(itdepth);
    printf("THEN\n");
    ir_stmts(s->then_part);
    if (s->else_part != NULL) {
	    tindent(itdepth);
        printf("ELSE\n");
        ir_stmts(s->else_part);
        }
    tindent(--itdepth);
    }

void ir_for(N_FOR * s, int nr) {
    tindent(itdepth);
    printf("FOR @%03d\n",nr);
    tindent(++itdepth);
    printf("ENTRY %s\n",s->loopvar->id);
    ir_expr(s->lb);
    ir_expr(s->ub);
    if (s->step!=NULL) {
        ir_expr(s->step);
        }
    ir_stmts(s->body);
	itdepth--;
    }

void ir_stmts(N_STMTLIST * stmts) {
    N_STMT * s;
	tindent(itdepth);
    printf("STMTLIST\n");
	itdepth++;
    for (s = stmts->first; s != NULL; s = s->next) {
        switch(s->typ) {
            case _ASSIGN:   
                ir_assign(s->me.s_assign,s->nr);
            break;
            case _IF:       
                ir_if(s->me.s_if,s->nr);
            break;
            case _FOR:      
                ir_for(s->me.s_for,s->nr);
            break;
            }
        }
	tindent(--itdepth);
    printf("/STMTLIST\n");
    }

