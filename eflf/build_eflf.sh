# 185.A64 Compilers for Parallel Systems SS 2018 H.Moritsch
# build EFLF

bison -d efl.y
flex efl.l
gcc -o eflf efl.tab.c lex.yy.c efl2f.c -L/usr/lib -lfl

