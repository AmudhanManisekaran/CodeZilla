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
            write(ParseTree).

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
    exprSet(Y), [/], exprSet(Z).

show(t_show(X)) --> [show], data(X).
readvar(t_read(X)) --> [read], id(X).

data(t_data(X)) --> str(X) ; id(X).

while(t_while(X, Y)) --> [while], bool(X), [do], cl(Y), [endwhile].

trad_for(t_trad_for(X,V,Y,Z,T)) --> [for], [open_para], 
    id(X), [equal], value(V),[semicolon],
    bool(Y), [semicolon], exprSet(Z), [close_para], [colon], cl(T).

for(t_for(U,X,Y,C)) --> [for], id(U),
    [inrange], [open_para], value(X), [colon], value(Y), [close_para],
    [colon], cl(C).

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
bool(t_not(X)) --> [not], bool(X).
bool(t_greaterthanequal(X,Y)) --> exprSet(X), [greater_thanequal], exprSet(Y).
bool(t_greaterthan(X,Y)) --> exprSet(X), [greater_than], exprSet(Y).
bool(t_equal(X, Y)) --> exprSet(X), [equalequal], exprSet(Y).
bool(t_lessthan(X,Y)) --> exprSet(X), [less_than], exprSet(Y).
bool(t_lessthanequal(X,Y)) --> exprSet(X), [less_thanequal], exprSet(Y).
bool(t_and(X,Y)) --> bool(X), [and], bool(Y).
bool(t_or(X,Y)) --> bool(X), [or], bool(Y).

