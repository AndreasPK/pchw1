#!/bin/sh

gfortran.exe $1 -o $1.exe -finit-local-zero
$1.exe > $1.log

