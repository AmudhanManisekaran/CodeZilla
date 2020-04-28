% =========== Parse tree generation ============== %
:- table expr/3, term/3, bool/3.

:- style_check(-singleton).

readFile(File, Final):-
    open(File, read, Stream),
    read(Stream,Final),
    close(Stream).

% Read the program from a file and returns the parse tree
codezilla(FileName) :-
    readFile(FileName, Tokens),
    parser(ParseTree, Tokens, []),
    eval(ParseTree, Z).

%====================================================================================================================
% =========== Parse tree generation ============== %
parser(t_parser(X)) --> program(X).
program(t_program(X)) --> [start],[semicolon],k(X),[end],[semicolon].

k(t_block(X,Y)) --> dl(X), [semicolon], cl(Y), [semicolon].

dl(t_declarationLINE(X,Y)) --> d(X), [semicolon], dl(Y).
dl(t_declarationLINE(X)) --> d(X).

d(t_declarationVAR(X)) --> [var], id(X).
d(t_declarationSTR(X)) --> [str], id(X).

cl(t_commandLINE(X,Y)) --> c(X), [semicolon], cl(Y).
cl(t_commandLINE(X)) --> c(X).

c(t_command_assign(X)) --> assign(X).
c(t_command_if(X)) --> if(X) ; ifelse(X).
c(t_command_ternary(X)) --> ternary(X).
c(t_command_loops(X)) --> loops(X).
c(t_command_show(X)) --> show(X).
c(t_command_read(X)) --> readvar(X).

assign(t_assign_str(X,Y)) --> id(X),[equal], str(Y).
assign(t_assign_var(X,Y)) --> id(X),[equal], exprSet(Y).

if(t_if(X,Y)) -->
    [if], bool(X), [then], cl(Y), [endif].

ifelse(t_ifelse(X,Y,Z)) -->
    [if], bool(X), [then], cl(Y), [else], cl(Z), [endif].

loops(t_loops(X)) -->  while(X); for(X); trad_for(X).

ternary(t_ternary(U,X,Y,Z)) --> id(U),[equal], bool(X), [ $ ],
    exprSet(Y), [/], exprSet(Z), [endternary].

show(t_show(X)) --> [show], data(X), [endshow].
readvar(t_read(X)) --> [read], id(X), [endread].

data(t_data(X)) --> str(X) ; id(X).

while(t_while(X, Y)) --> [while], bool(X), [do], cl(Y), [endwhile].

trad_for(t_trad_for(X,V,Y,Z,T)) --> [for], [open_para],
    id(X), [equal], value(V),[semicolon],
    bool(Y), [semicolon], exprSet(Z), [close_para], [colon], cl(T), [endfor].

for(t_for(U,X,Y,C)) --> [for], id(U),
    [inrange], [open_para], value(X), [colon], value(Y), [close_para],
    [colon], cl(C), [endfor].

exprSet(t_expr(X)) --> expr(X).
exprSet(t_assign(I,E)) --> id(I),[equal], exprSet(E).

expr(t_add(X,Y)) --> expr(X), [+], term(Y).
expr(t_sub(X,Y)) --> expr(X), [-], term(Y).
expr(X) --> term(X).

term(t_div(X,Y)) --> term(X), [/], fact(Y).
term(t_mul(X,Y)) --> term(X), [*], fact(Y).
term(X) --> fact(X).

fact(t_fact_para(X)) --> [open_para], exprSet(X), [close_para].
fact(t_fact(X)) --> value(X).

value(t_value(X)) --> id(X); int1(X) ; float(X).

int1(t_int(X)) --> [X], {integer(X)}.
float(t_float(X)) --> [X], {float(X)}.
id(t_id(I)) --> [I], {atom(I)}.

str(t_str(S)) --> [less_than],[less_than],
    [S], {atom(S)},
    [greater_than],[greater_than].

bool(true) --> [true].
bool(false) --> [false].
bool(t_not(X)) --> [not], [open_para], bool(X), [close_para].
bool(t_greaterthanequal(X,Y)) --> exprSet(X), [greater_thanequal], exprSet(Y).
bool(t_greaterthan(X,Y)) --> exprSet(X), [greater_than], exprSet(Y).
bool(t_equal(X, Y)) --> exprSet(X), [equalequal], exprSet(Y).
bool(t_lessthan(X,Y)) --> exprSet(X), [less_than], exprSet(Y).
bool(t_lessthanequal(X,Y)) --> exprSet(X), [less_thanequal], exprSet(Y).
bool(t_and(X,Y)) --> bool(X), [and], bool(Y).
bool(t_or(X,Y)) --> bool(X), [or], bool(Y).

% ================= Evaluation =================== %

/* program_eval is used to update the input values and evaluate
 * the parse tree of the given code. */
eval(t_parser(K),Z) :- evalProgram(K,Z).

evalProgram(t_program(K), Z):-
    evalBlock(K,_Env,New_env),
    Z = New_env, !.

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
    evalCL(Y,Env1,Env2).

/* evalDL is used to evaluate the declaration lines consisting of multiple
 * declarations */
evalDL(t_declarationLINE(X,Y), Env, Env2):-
    evalDEC(X, Env, Env1),
    evalDL(Y, Env1, Env2).
evalDL(t_declarationLINE(X), Env, Env2):-
    evalDEC(X, Env, Env2).

/* evalDEC is used to evaluate the individual declarations */
evalDEC(t_declarationVAR(t_id(X)), Env, Env1):-
    update(X, 0, Env, Env1).
evalDEC(t_declarationSTR(t_id(X)), Env, Env1):-
    update(X, null, Env, Env1).

/* evalCL is used to evaluate the command lines consisting of multiple
 * commands */
evalCL(t_commandLINE(X,Y), Env, Env2):-
    evalCMD(X, Env, Env1),
    evalCL(Y, Env1, Env2).
evalCL(t_commandLINE(X), Env, Env1):-
    evalCMD(X, Env, Env1).


/* evalCMD is used to evaluate the individual commands */

evalCMD(t_command_assign(X),Env,Env1) :-
    evalAssign(X,Env,Env1).

evalCMD(t_command_if(X),Env,Env1) :-
    evalIfElse(X,Env,Env1);
    evalIf(X,Env,Env1).

evalCMD(t_command_show(X),Env,Env) :- evalShow(X,Env).

evalCMD(t_command_read(X),Env,Env1) :- evalRead(X,Env,Env1).

evalCMD(t_command_loops(X), Env, Env1):- evalLoops(X,Env,Env1).

evalCMD(t_command_ternary(X), Env, Env1):- evalTernary(X,Env,Env1).

evalTernary(t_ternary(t_id(U),X,Y,_Z),Env,Env2):-
            evalBOOL(X,Env,true),
            evalExprSet(Y,Env,Env1,Val),
            update(U,Val,Env1,Env2).

evalTernary(t_ternary(t_id(U),X,_Y,Z),Env,Env2):-
            evalBOOL(X,Env,false),
            evalExprSet(Z,Env,Env1,Val),
            update(U,Val,Env1,Env2).

evalLoops(t_loops(X),Env,Env1) :- evalWhile(X,Env,Env1);
    evalFor(X,Env,Env1);
    evalTradFor(X, Env, Env1).

evalShow(t_show(X),Env):- evalData(X,Env).

evalData(t_data(X),Env):- evalDataId(X,Env); evalDataString(X,Env).

evalDataId(t_id(X),Env):-
    lookup(X,Env,Val),nl,write(X), write(' = '), write(Val).

evalDataString(t_str(X),_Env):- nl, write(X).

evalRead(t_read(t_id(X)),Env,Env1):-
  nl,
  write('please enter value for:'), write(X),
  nl,
  read(Z),
  update(X,Z,Env,Env1).

evalWhile(t_while(X,Y), Env, Env2):-
    evalBOOL(X,Env,true),
    evalCL(Y, Env, Env1),
    evalWhile(t_while(X,Y), Env1, Env2).

evalWhile(t_while(X,_Y), Env, Env):-
    evalBOOL(X, Env, false).

evalAssign(t_assign_var(t_id(X),Y),Env, Env2):-
    evalExprSet(Y, Env, Env1, Val),
    update(X, Val, Env1, Env2).

evalAssign(t_assign_str(t_id(X),t_str(Y)),Env, Env2):-
    update(X, Y, Env, Env2).

evalIfElse(t_ifelse(X,_Y,Z), Env, Env1):-
    evalBOOL(X,Env,false),
    evalCL(Z, Env, Env1).
evalIfElse(t_ifelse(X,Y,_Z), Env, Env1):-
    evalBOOL(X,Env,true),
    evalCL(Y, Env, Env1).

evalIf(t_if(X,_Y), Env, Env):-
    evalBOOL(X,Env,Val),
    Val = false.

evalIf(t_if(X,Y), Env, Env1):-
    evalBOOL(X,Env,Val),
    Val = true,
    evalCL(Y, Env, Env1).


evalFor(t_for(t_id(U),t_value(t_int(X)),t_value(t_int(Y)),C), Env, Env3):-
    update(U, X, Env, Env1),
    lookup(U, Env1, U_Val),
    U_Val < Y,
    evalCL(C, Env1, Env2),
    U1 is U_Val + 1,
    evalFor(t_for(t_id(U),t_value(t_int(U1)),t_value(t_int(Y)),C), Env2, Env3).

evalFor(t_for(_U,t_value(t_int(X)),t_value(t_int(Y)),_C), Env, Env):-
    X >= Y.

evalTradFor(t_trad_for(t_id(X),t_value(t_int(V)),Y,_Z,_T), Env, Env1):-
    update(X, V, Env, Env1),
    evalBOOL(Y, Env1, false).

evalTradFor(t_trad_for(t_id(X),t_value(t_int(V)),Y,Z,T), Env, Env4):-
    update(X, V, Env, Env1),

    evalBOOL(Y, Env1, true),

    evalCL(T, Env1, Env2),

    evalExprSet(Z,Env2, Env3, _X_Val),

    evalTradForTwo(t_trad_for(t_id(X),Y,Z,T), Env3, Env4).

evalTradForTwo(t_trad_for(t_id(_X),Y,_Z,_T), Env, Env):-
    evalBOOL(Y, Env, false).

evalTradForTwo(t_trad_for(t_id(X),Y,Z,T), Env1, Env4):-
    evalBOOL(Y, Env1, true),
    evalCL(T, Env1, Env2),
    evalExprSet(Z,Env2, Env3, _X_Val),
    evalTradForTwo(t_trad_for(t_id(X),Y,Z,T), Env3, Env4).

/* evalExprSet is used to handle double assignments */
evalExprSet(t_expr(X),Env, Env2, Val):-
    evalEXPR(X, Env, Env2, Val).
evalExprSet(t_assign(t_id(I),E),Env,Env2,Val):-
    evalExprSet(E, Env, Env1, Val),
    update(I, Val, Env1, Env2).

/* evalEXPR is used to evalute the addition and subtraction expressions*/
evalEXPR(t_add(X,Y), Env, Env, Val) :-
   evalEXPR(X, Env, Env, Val1),
   evalTERM(Y, Env, Env, Val2),
   Val is Val1 + Val2.
evalEXPR(t_sub(X,Y), Env, Env2,Val) :-
    evalEXPR(X, Env, Env1, Val1),
    evalTERM(Y, Env1, Env2, Val2),
    Val is Val1 - Val2.
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
evalFACT(t_fact_para(X),Env, Env, Val) :-
    evalExprSet(X, Env, Env, Val).
evalFACT(t_fact(X),Env, Env, Val) :-
    evalValue(X, Env, Val).

/* evalNUM is used to match variables and numbers */
evalValue(t_value(X),Env,Val) :-
    evalINT(X, Env, Val);
    evalFLOAT(X, Env, Val);
    evalID(X, Env,Val).

evalID(t_id(I), Env, Val) :-
    lookup(I, Env, Val).
evalINT(t_int(X), _Env, X).
evalFLOAT(t_float(X), _Env, X).


/* evalBOOL is used to evaluate statements which have truth values */
evalBOOL(false, _Env, false).
evalBOOL(true, _Env, true).
evalBOOL(t_not(B), Env, Val) :-
    evalBOOL(B, Env, Val1),
    not(Val1, Val).
evalBOOL(t_equal(E1,E2), Env, Val) :-
    evalExprSet(E1, Env, Env1, Val1),
    evalExprSet(E2, Env, Env1, Val2),
    equal(Val1, Val2, Val).

evalBOOL(t_and(E1,E2), Env, Val) :-
    evalBOOL(E1, Env, Val1),
    evalBOOL(E2, Env, Val2),
    and_func(Val1,Val2,Val).

evalBOOL(t_or(E1,E2), Env, Val) :-
    evalBOOL(E1, Env, Val1),
    evalBOOL(E2, Env, Val2),
    or_func(Val1,Val2,Val).

evalBOOL(t_lessthan(E1,E2), Env, Val) :-
    evalExprSet(E1, Env, Env1, Val1),
    evalExprSet(E2, Env, Env1, Val2),
    lessthan(Val1, Val2, Val).

evalBOOL(t_lessthanequal(E1,E2), Env, Val) :-
    evalExprSet(E1, Env, Env1, Val1),
    evalExprSet(E2, Env, Env1, Val2),
    lessthanequal(Val1, Val2, Val).

evalBOOL(t_greaterthan(E1,E2), Env, Val) :-
    evalExprSet(E1, Env, Env1, Val1),
    evalExprSet(E2, Env, Env1, Val2),
    greaterthan(Val1, Val2, Val).

evalBOOL(t_greaterthanequal(E1,E2), Env, Val) :-
    evalExprSet(E1, Env, Env1, Val1),
    evalExprSet(E2, Env, Env1, Val2),
    greaterthanequal(Val1, Val2, Val).

not(false, true).
not(true, false).

and_func(true,true,X):- X = true.
and_func(true,false,X):- X = false.
and_func(false,true,X):- X = false.
and_func(false,false,X):- X = false.

or_func(true,true,X):- X = true.
or_func(true,false,X):- X = true.
or_func(false,true,X):- X = true.
or_func(false,false,X):- X = false.

/* equal is used to check if two entities are numerically equal */
equal(Val1, Val2, false):- Val1 \= Val2.
equal(Val1, Val2, true):- Val1 = Val2.

lessthan(Val1, Val2, true):- Val1 < Val2.
lessthan(Val1, Val2, false):- Val1 >= Val2.

lessthanequal(Val1, Val2, true):- Val1 =< Val2.
lessthanequal(Val1, Val2, false):- Val1 > Val2.

greaterthan(Val1, Val2, true):- Val1 > Val2.
greaterthan(Val1, Val2, false):- Val1 =< Val2.

greaterthanequal(Val1, Val2, true):- Val1 >= Val2.
greaterthanequal(Val1, Val2, false):- Val1 < Val2.
