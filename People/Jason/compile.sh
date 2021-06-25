#!/bin/bash
lhs2TeX Typing.lhs -o Typing.tex 
pdflatex -interaction=nonstopmode Typing.tex
