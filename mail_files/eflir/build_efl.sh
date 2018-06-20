# 185.A64 Compilers for Parallel Systems SS 2016 H.Moritsch
# build EFLF

bison -d efl.y
flex efl.l
gcc -o efl efl.tab.c lex.yy.c efl2f.c efl2ir.c -lfl

