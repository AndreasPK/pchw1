#!/usr/bin/env bash
set -x
../eflf.exe < ${1}.efl > ${1}.f90
gfortran ${1}.f90 -o ${1} -finit-local-zero
./${1}.exe > ${1}.run.out 