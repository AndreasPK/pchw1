/* 185.A64 Compilers for Parallel Systems SS 2016 H.Moritsch
   EFL Intermediate Representation
*/

#define MAXRANK 8

#define FIRST 0
#define LAST  1

typedef enum { FALSE=0, TRUE } BOOL;

typedef enum { _INT=0, _FLOAT } DATA_TYPE;
typedef enum { _ANTI=-1, _OUT, _TRUE } DEP_TYPE;

/* Array description
    - number of dimensions
    - lower and upper bounds in each dimension
*/
typedef struct tARR_SIZE {
        int rank;               /* number of dimensions */
        int lb[MAXRANK+1];      /* lower bound(s) */
        int ub[MAXRANK+1];      /* upper bound(s) */
        } ARR_SIZE;

/* Symbol table entry. The symbol table is implemented as
   a list of symbol table entries.
*/
typedef struct tENTRY {
    char      * id ;            /* name */
    DATA_TYPE data_type;        /* data type */
    enum { _SCAL=0, _ARR} dim_type ;    /* scalar or array */
    ARR_SIZE * size ;
/*
    Next entry in the symbol table
*/
    struct tENTRY *next;
    } ENTRY;

/*
    Types of syntax tree nodes N_<type>
*/

/* 1. Reference to a (scalar or array) variable
*/
typedef struct tN_VAR {
    ENTRY * entry;              /* symbol table entry of the variable */
    struct tN_EXPRLIST * index; /* index expression(s) */
    } N_VAR;

/* 2. Expression
*/
typedef struct tN_EXPR {
    enum { _FLOATNUM=0, _INTNUM, _VAR, _OP } typ;
    union {
        float float_number;     /* float number */
        int   int_number;       /* integer number */
        N_VAR * var_ref;        /* reference to a variable */
        struct {
            enum { NO_OP=0, EQ_OP, NE_OP, GT_OP, GE_OP, LT_OP, LE_OP,
                   PLUS_OP, MINUS_OP, MULT_OP, DIV_OP,
                   AND_OP, OR_OP, NOT_OP } oper;    /* operator */
            struct tN_EXPR * op1expr;   /* 1st operand */
            struct tN_EXPR * op2expr;   /* 2nd operand */
            } op;
        } me;
    struct tN_EXPR * next;      /* expression list */
    } N_EXPR;

/* List of expressions
*/
typedef struct tN_EXPRLIST {
    N_EXPR * first;
    N_EXPR * last;
    } N_EXPRLIST;

/* 3. Assignment statement
*/
typedef struct tN_ASSIGN {
    N_VAR  * var_ref;       /* reference to a variable */
    N_EXPR * expr;          /* right hand side expression */
    } N_ASSIGN;

/* 4. If statement
*/
typedef struct tN_IF {
    N_EXPR * expr;          /* condition expression */
    struct tN_STMTLIST * then_part;
    struct tN_STMTLIST * else_part;
    } N_IF;

/* 5. For loop
*/
typedef struct tN_FOR {
    ENTRY  * loopvar;           /* loop variable */
    N_EXPR * lb;                /* lower bound */
    N_EXPR * ub;                /* upper bound */
    N_EXPR * step;              /* step */
    struct tN_STMTLIST * body;  /* loop body */
    } N_FOR;

/* 6. Statement
*/
typedef struct tN_STMT {
    enum { _ASSIGN=0, _IF, _FOR } typ;
    union {
        N_ASSIGN * s_assign;
        N_IF     * s_if;
        N_FOR    * s_for;
        } me;
    int nr;                     /* statement number */
    struct tN_STMT * next;      /* statement list */
    } N_STMT;

/* List of statements
*/
typedef struct tN_STMTLIST {
    N_STMT * first;
    N_STMT * last;
    } N_STMTLIST;

/* 7. Root node of the program's AST
*/
typedef struct tN_PROG {
    char        * name;
    ENTRY   ** symbols;         /* Symbol table */
    N_STMTLIST * stmts;         /* Executable statments */
    } N_PROG;

N_PROG * prog;
int stmt_count;
int loop_count;
