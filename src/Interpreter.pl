% =========== Parse tree generation ============== %

/* program represents the whole program */
program(t_program(X)) --> k(X),[.].

/* k represents each block */
k(t_block(X,Y)) --> [begin], dl(X), [;], cl(Y),[end].

/* dl represents each declaration line */
dl(t_declarationLINE(X,Y)) --> d(X), [;], dl(Y).
dl(t_declarationLINE(X)) --> d(X).

/* d represents each declaration */
d(t_declarationCONS(X,Y)) --> [const], id(X), [=], num(Y).
d(t_declarationVAR(X)) --> [var], id(X).

/* cl represents each command line */
cl(t_commandLINE(X,Y)) --> c(X), [;], cl(Y).
cl(t_commandLINE(X)) --> c(X).

/* c represents each command */
c(t_commandASSIGN(X,Y)) --> id(X), [:=], exprSet(Y).
c(t_commandIFELSE(X,Y,Z)) --> 
    [if], bool(X), [then], cl(Y), [else], cl(Z), [endif].
c(t_commandWHILE(X, Y)) --> [while], bool(X), [do], cl(Y), [endwhile].
c(t_commandBLOCK(X)) --> k(X).

exprSet(t_expr(X)) --> expr(X).
exprSet(t_assign(I,E)) --> id(I), [:=], exprSet(E).

/* expr is used to generate the parse tree of the expressions */
expr(t_add(X,Y)) --> expr(X), [+], term(Y).
expr(t_sub(X,Y)) --> expr(X), [-], term(Y).
expr(X) --> term(X).

/* term is used to generate the parse tree of the terms */
term(t_div(X,Y)) --> term(X), [/], fact(Y).
term(t_mul(X,Y)) --> term(X), [*], fact(Y).
term(X) --> fact(X).

/* fact is used to generate the parse tree of the terms */
fact(t_fact(X)) --> num(X).
fact(t_fact(X)) --> id(X).
fact(t_fact(X)) --> ['('], exprSet(X), [')'].

/* num is used to generate the parse tree of numbers and variables */
num(t_num(X)) --> [X], {number(X)}.
id(t_id(I)) --> [I], {atom(I)}.

/* bool represents boolean expressions */
bool(true) --> [true].
bool(false) --> [false].
bool(t_not(X)) --> [not], bool(X).
bool(t_equal(X, Y)) --> expr(X), [=], expr(Y).
