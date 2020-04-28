:- style_check(-singleton).

codezilla(FileName):-
    open(FileName, read, InStream),
    read(InStream, X),
    close(InStream),
    eval(X, Z).

%====================================================================================================================
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
