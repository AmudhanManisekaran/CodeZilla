@echo off
Rem # @author {Mayank Rawat}, version 1.0
Rem # @purpose {This Script runs lexer, compiler and interpreter in sequence in windows}


START /B /wait python lexer.py

START /B /wait swipl compiler.pl

START /B /wait swipl interpreter.pl

START /B /wait python remove.py