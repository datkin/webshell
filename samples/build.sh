#!/bin/bash

gcc main.c -lcurses
./a.out

gcc -std=c99 -Wall tty.c
