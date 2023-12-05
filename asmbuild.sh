#!/bin/bash
nasm -f elf64 -o $1.o $1.s
gcc -c asmprint.c -o asmprint.o
gcc -no-pie -o $1 $1.o asmprint.o
rm $1.o
./$1