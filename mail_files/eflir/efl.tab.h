/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_EFL_TAB_H_INCLUDED
# define YY_YY_EFL_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    PROGRAM = 258,
    END = 259,
    INT = 260,
    FLOAT = 261,
    IF = 262,
    THEN = 263,
    ELSE = 264,
    ENDIF = 265,
    FOR = 266,
    TO = 267,
    STEP = 268,
    ENDFOR = 269,
    ASSIGN = 270,
    EQ = 271,
    NE = 272,
    GT = 273,
    GE = 274,
    LT = 275,
    LE = 276,
    PLUS = 277,
    MINUS = 278,
    TIMES = 279,
    OVER = 280,
    AND = 281,
    OR = 282,
    NOT = 283,
    LBR = 284,
    RBR = 285,
    LPAR = 286,
    RPAR = 287,
    COMMA = 288,
    COLON = 289,
    QUOT = 290,
    EOL = 291,
    ID = 292,
    FCONST = 293,
    ICONST = 294
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 31 "efl.y" /* yacc.c:1909  */

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

#line 113 "efl.tab.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_EFL_TAB_H_INCLUDED  */
