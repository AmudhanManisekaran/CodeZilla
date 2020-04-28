% @authors {Sandya Manoharan}, @version 1.4
% @purpose Parsing the tokens to obtain parse tree
% @version 1.4
% @date 04/28/20

/* table to eliminate left recursion */
:- table expr/3, term/3, bool/3.

:- style_check(-singleton).

/* Read the program from a file and returns the parse tree  */
readFile(File, Final):-
    open(File, read, Stream),
    read(Stream,Final),
    close(Stream).

codezilla(FileName) :-
    working_directory(_, '../../data'),
    readFile(FileName, Tokens),
    parser(ParseTree, Tokens, []),
    split_string(FileName, ".", "", L),
    L = [H|_T],
    atom_concat(H, ".pt", X),
    open(X, write, OutStream),
    write(OutStream, ParseTree),
    write(OutStream, '.'),
    close(OutStream).

% =========== Parse tree generation ============== %

/* program represents the whole program */
parser(t_parser(X)) --> program(X).
program(t_program(X)) --> [start],[semicolon],k(X),[end],[semicolon].

/* k represents each block */
k(t_block(X,Y)) --> dl(X), [semicolon], cl(Y), [semicolon].

/* dl represents each declaration line */
dl(t_declarationLINE(X,Y)) --> d(X), [semicolon], dl(Y).
dl(t_declarationLINE(X)) --> d(X).

/* d represents each declaration */
d(t_declarationVAR(X)) --> [var], id(X).
d(t_declarationSTR(X)) --> [str], id(X).

/* cl represents each command line */
cl(t_commandLINE(X,Y)) --> c(X), [semicolon], cl(Y).
cl(t_commandLINE(X)) --> c(X).

/* c represents each command */
c(t_command_assign(X)) --> assign(X).
c(t_command_if(X)) --> if(X) ; ifelse(X).
c(t_command_ternary(X)) --> ternary(X).
c(t_command_loops(X)) --> loops(X).
c(t_command_show(X)) --> show(X).
c(t_command_read(X)) --> readvar(X).

/* assign represents various types of assignment operations */
assign(t_assign_str(X,Y)) --> id(X),[equal], str(Y).
assign(t_assign_var(X,Y)) --> id(X),[equal], exprSet(Y).
assign(t_assign_bool(X,Y)) --> id(X),[equal], bool(Y).

/ * if represents the regular conditional structure */
if(t_if(X,Y)) -->
    [if], bool(X), [then], cl(Y), [endif].

/ * ifelse represents the alternative conditional structure */
ifelse(t_ifelse(X,Y,Z)) -->
    [if], bool(X), [then], cl(Y), [else], cl(Z), [endif].

/ * loops represents the looping constructs */
loops(t_loops(X)) -->  while(X); for(X); trad_for(X).

/ * ternary represents the ternary operator */
ternary(t_ternary(U,X,Y,Z)) --> id(U),[equal], bool(X), [ $ ],
    exprSet(Y), [/], exprSet(Z), [endternary].

/ * show represents the out statement and read represents the in statement */
show(t_show(X)) --> [show], data(X), [endshow].
readvar(t_read(X)) --> [read], id(X), [endread].

data(t_data(X)) --> str(X) ; id(X).

/* while represents a type of looping construct  */
while(t_while(X, Y)) --> [while], bool(X), [do], cl(Y), [endwhile].

/* trad_for represents the traditional for loop  */
trad_for(t_trad_for(X,V,Y,Z,T)) --> [for], [open_para],
    id(X), [equal], value(V),[semicolon],
    bool(Y), [semicolon], exprSet(Z), [close_para], [colon], cl(T), [endfor].

/* for represents the for loop with range */
for(t_for(U,X,Y,C)) --> [for], id(U),
    [inrange], [open_para], value(X), [colon], value(Y), [close_para],
    [colon], cl(C), [endfor].

/* exprset is used to generate the parse tree of the expressions */
exprSet(t_expr(X)) --> expr(X).
exprSet(t_assign(I,E)) --> id(I),[equal], exprSet(E).

/* expr is used to generate the parse tree of the expressions */
expr(t_add(X,Y)) --> expr(X), [+], term(Y).
expr(t_sub(X,Y)) --> expr(X), [-], term(Y).
expr(X) --> term(X).

/* term is used to generate the parse tree of the terms */
term(t_div(X,Y)) --> term(X), [/], fact(Y).
term(t_mul(X,Y)) --> term(X), [*], fact(Y).
term(X) --> fact(X).

/* fact is used to generate the parse tree of the terms */
fact(t_fact_para(X)) --> [open_para], exprSet(X), [close_para].
fact(t_fact(X)) --> value(X).

/* value represents the possible values that an attribute can take */
value(t_value(X)) --> id(X); int1(X) ; float(X).

int1(t_int(X)) --> [X], {integer(X)}.
float(t_float(X)) --> [X], {float(X)}.
id(t_id(I)) --> [I], {atom(I)}.

/* str represents the string data type */
str(t_str(S)) --> [less_than],[less_than],
    [S], {atom(S)},
    [greater_than],[greater_than].

/* bool represents boolean expressions */
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
