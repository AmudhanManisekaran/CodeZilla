% =========== Parse tree generation ============== %

/* program represents the whole program */
program(t_program(X)) --> [start],[;],k(X),[end],[;].

/* k represents each block */
k(t_block(X,Y)) --> dl(X), [;], cl(Y),[;].

/* dl represents each declaration line */
dl(t_declarationLINE(X,Y)) --> d(X), [;], dl(Y).
dl(t_declarationLINE(X)) --> d(X).

/* d represents each declaration */
d(t_declarationVAR(X)) --> [var], id(X).
d(t_declarationSTR(X)) --> [str], id(X).
/* cl represents each command line */
cl(t_commandLINE(X,Y)) --> c(X), [;], cl(Y).
cl(t_commandLINE(X)) --> c(X).

c(t_command_assign(X)) --> assign(X).
c(t_command_if(X)) --> if(X) ; ifelse(X).
c(t_command_ternary(X)) --> ternary(X).
c(t_command_loops(X)) --> loops(X).
c(t_command_show(X)) --> show(X).
c(t_command_read(X)) --> read(X).
c(t_command_block(X)) --> k(X).

assign(t_assign_str(X,Y)) --> id(X), [:=], str(Y).
assign(t_assign_var(X,Y)) --> id(X), [:=], exprSet(Y).


if(t_if(X,Y)) -->
    [if], bool(X), [then], cl(Y), [endif].

ifelse(t_if(X,Y,Z)) -->
    [if], bool(X), [then], cl(Y), [else], cl(Z), [endif].

ternary(t_ternary(U,X,Y,Z)) --> id(U), [:=], bool(X), [$], 
    exprSet(Y), [/], exprSet(Z). 
        
loops(t_loops(X)) -->  while(X); for(X); trad_for(X).

show(t_show(X)) --> [show], data(X).
read(t_read(X)) --> [read], id(X).

data(t_data(X)) --> str(X) ; id(X).

while(t_while(X, Y)) --> [while], bool(X), [do], cl(Y), [endwhile].

trad_for(t_trad_for(X,V,Y,Z)) --> [for], ['('], id(X), [:=], value(V),[;], 
    bool(Y), [;], exprSet(Z), [')'].

for(t_for(U,X,Y,C)) --> [for], id(U), 
    [inrange], ['('], value(X), value(Y), [')'],
    [do], cl(C).
    
exprSet(t_expr(X)) --> expr(X).
exprSet(t_assign(I,E)) --> id(I), [:=], exprSet(E).

:- table expr/3, term/3.

/* expr is used to generate the parse tree of the expressions */
expr(t_add(X,Y)) --> expr(X), [+], term(Y).
expr(t_sub(X,Y)) --> expr(X), [-], term(Y).
expr(X) --> term(X).

/* term is used to generate the parse tree of the terms */
term(t_div(X,Y)) --> term(X), [/], fact(Y).
term(t_mul(X,Y)) --> term(X), [*], fact(Y).
term(X) --> fact(X).

/* fact is used to generate the parse tree of the terms */
fact(t_fact(X)) --> value(X).
fact(t_fact(X)) --> ['('], exprSet(X), [')'].

/* num is used to generate the parse tree of numbers and variables */
value(t_value(X)) --> id(X); num(X) ; float(X).

num(t_num(X)) --> [X], {number(X)}.
float(t_float(X)) --> [X], {float(X)}.
id(t_id(I)) --> [I], {atom(I)}.

str(t_str(I)) --> [<<],[I], {string(I)},[>>].

/* bool represents boolean expressions */
bool(true) --> [true].
bool(false) --> [false].
bool(t_not(X)) --> [not], bool(X).
bool(t_and(X,Y)) --> expr(X), [and], bool(Y).
bool(t_or(X,Y)) --> expr(X), [or], bool(Y).
bool(t_boolcondition(X)) --> compare_bool(X).

compare_bool(t_equal(X, Y)) --> expr(X), [=:=], expr(Y).
compare_bool(t_lessthan(X,Y)) --> expr(X), [<], expr(Y).
compare_bool(t_greaterthan(X,Y)) --> expr(X), [>], expr(Y).
compare_bool(t_lessthanequal(X,Y)) --> expr(X), [<=], expr(Y).
compare_bool(t_greaterthanequal(X,Y)) --> expr(X), [>=], expr(Y).
% ================= Evaluation =================== %

/* eliminating left recursion in predicates expr and term */
:- table expr/3, term/3.

/* program_eval is used to update the input values and evaluate
 * the parse tree of the given code. */

program_eval(t_program(K), X, Y, Z):-
    update(x,X,_Env,Env1),
    update(y,Y,Env1,Env2),
    evalBlock(K,Env2,New_env),
    lookup(z,New_env,Z), !.

/* Here, Cut is used to avoid the addition of garbage value to the
 * environment after one complete evaluation */

/* update is used to revise the value of a variable in the environment */
update(Id, Val, [], [(Id, Val)]).
update(Id, Val, [(Id, _)|T], [(Id, Val)|T]).
update(Id, Val, [H|T], [H|R]):-
    H \= (Id,_),
    update(Id, Val, T, R).

/* lookup is used to obtain the values from environment */
lookup(Id, [(Id, Val)|_], Val).
lookup(Id, [_|T], Val) :-
    lookup(Id, T, Val).

/* eval_block is used to evaluate any block of code in the program */
evalBlock(t_block(X,Y), Env, Env2):-
    evalDL(X, Env, Env1),
    evalCL(Y, Env1, Env2).

/* evalDL is used to evaluate the declaration lines consisting of multiple
 * declarations */
evalDL(t_declarationLINE(X,Y), Env, Env2):-
    evalDEC(X, Env, Env1),
    evalDL(Y, Env1, Env2).
evalDL(t_declarationLINE(X), Env, Env2):-
    evalDEC(X, Env, Env2).

/* evalDEC is used to evaluate the individual declarations */
evalDEC(t_declarationCONS(X,Y), Env, Env1):-
    update(X, Y, Env, Env1).
evalDEC(t_declarationVAR(_), Env, Env).

/* evalCL is used to evaluate the command lines consisting of multiple
 * commands */
evalCL(t_commandLINE(X,Y), Env, Env2):-
    evalCMD(X, Env, Env1),
    evalCL(Y, Env1, Env2).
evalCL(t_commandLINE(X), Env, Env1):-
    evalCMD(X, Env, Env1).

/* evalCMD is used to evaluate the individual commands */
evalCMD(t_commandASSIGN(t_id(X),Y),Env, Env2):-
    evalExprSet(Y, Env, Env1, Val),
    update(X, Val, Env1, Env2).

evalCMD(t_commandIFELSE(X,_Y,Z), Env, Env1):-
    evalBOOL(X,Env,false),
    evalCL(Z, Env, Env1).
evalCMD(t_commandIFELSE(X,Y,_Z), Env, Env1):-
    evalBOOL(X,Env,true),
    evalCL(Y, Env, Env1).

evalCMD(t_commandWHILE(X,_Y), Env, Env):-
    evalBOOL(X, Env, false).
evalCMD(t_commandWHILE(X,Y), Env, Env2):-
    evalBOOL(X,Env,true),
    evalCL(Y, Env, Env1),
    evalCMD(t_commandWHILE(X,Y), Env1, Env2).

evalCMD(t_commandBLOCK(X),Env,Env2):- evalBlock(X, Env, Env2).

/* evalExprSet is used to handle double assignments */
evalExprSet(t_expr(X),Env, Env2, Val):-
    evalEXPR(X, Env, Env2, Val).
evalExprSet(t_assign(I,E),Env,Env2,Val):-
    evalExprSet(E, Env, Env1, Val),
    update(I, Val, Env1, Env2).

/* evalEXPR is used to evalute the addition and subtraction expressions*/
evalEXPR(t_add(X,Y), Env, Env, Val) :-
   evalEXPR(X, Env, Env, Val1),
   evalTERM(Y, Env, Env, Val2),
   Val is Val1 + Val2.
evalEXPR(t_sub(X,Y), Env, Env2,Val) :-
    evalEXPR(X, Env, Env1, Val1),
    evalTERM(Y, Env1, Env2, Val2),Val is Val1 - Val2.
evalEXPR(X, Env, Env1, Val) :-
    evalTERM(X, Env, Env1, Val).

/* evalTERM is used to evalute multiplication and division expressions */
evalTERM(t_div(X,Y), Env, Env2, Val) :-
    evalTERM(X, Env, Env1, Val1),
    evalFACT(Y, Env1, Env2, Val2),
    Val is Val1 / Val2.
evalTERM(t_mul(X,Y), Env, Env2, Val) :-
    evalTERM(X, Env, Env1, Val1),
    evalFACT(Y, Env1, Env2, Val2),
    Val is Val1 * Val2.
evalTERM(X, Env, Env, Val) :-
    evalFACT(X, Env, Env, Val).

/* evalFACT is used to evalute the expressions and match to values or
 * extension of double assignments */
evalFACT(t_fact(X),Env, Env, Val) :-
    evalNUM(X, Env, Val).
evalFACT(t_fact(X),Env, Env, Val) :-
    evalExprSet(X, Env, Env, Val).

/* evalNUM is used to match variables and numbers */
evalNUM(t_num(X), _Env, X).
evalNUM(t_id(I), Env, Val) :-
    lookup(I, Env, Val).

/* evalBOOL is used to evaluate statements which have truth values */
evalBOOL(false, _Env, false).
evalBOOL(true, _Env, true).
evalBOOL(t_equal(E1,E2), Env, Val) :-
    evalEXPR(E1, Env, Env1, Val1),
    evalEXPR(E2, Env, Env1, Val2),
    equal(Val1, Val2, Val).
evalBOOL(t_not(B), Env, Val) :-
    evalBOOL(B, Env, Val1),
    not(Val1, Val).

/* not is used toggle between truth values */
not(false, true).
not(true, false).

/* equal is used to check if two entities are numerically equal */
equal(Val1, Val2, false):- Val1 \= Val2.
equal(Val1, Val2, true):- Val1 = Val2.
