% =========== Parse tree generation ============== %
:- table expr/3, term/3.

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
program(t_program(X)) --> [start],[SEMICOLON],k(X),[end],[SEMICOLON].

k(t_block(X,Y)) --> dl(X), [SEMICOLON], cl(Y), [SEMICOLON].

dl(t_declarationLINE(X,Y)) --> d(X), [SEMICOLON], dl(Y).
dl(t_declarationLINE(X)) --> d(X).

d(t_declarationVAR(X)) --> [var], id(X).
d(t_declarationSTR(X)) --> [str], id(X).

cl(t_commandLINE(X,Y)) --> c(X), [SEMICOLON], cl(Y).
cl(t_commandLINE(X)) --> c(X).

c(t_command_assign(X)) --> assign(X).
c(t_command_if(X)) --> if(X) ; ifelse(X).
/*c(t_command_ternary(X)) --> ternary(X).*/
c(t_command_loops(X)) --> loops(X).
c(t_command_show(X)) --> show(X).
c(t_command_read(X)) --> readvar(X).
c(t_command_block(X)) --> k(X).

assign(t_assign_str(X,Y)) --> id(X),[EQUAL], str(Y).
assign(t_assign_var(X,Y)) --> id(X),[EQUAL], exprSet(Y).

if(t_if(X,Y)) -->
    [if], bool(X), [then], cl(Y), [endif].

ifelse(t_ifelse(X,Y,Z)) -->
    [if], bool(X), [then], cl(Y), [else], cl(Z), [endif].

/*ternary(t_ternary(U,X,Y,Z)) --> id(U),[EQUAL], bool(X), [ $ ],
    exprSet(Y), [/], exprSet(Z).
*/

loops(t_loops(X)) -->  while(X); for(X); trad_for(X).

show(t_show(X)) --> [show], data(X).
readvar(t_read(X)) --> [readvar], id(X).

data(t_data(X)) --> str(X) ; id(X).

while(t_while(X, Y)) --> [while], bool(X), [do], cl(Y), [endwhile].

trad_for(t_trad_for(X,V,Y,Z,T)) --> [for], [OPEN_PARA], id(X), [EQUAL], value(V),[SEMICOLON],
    bool(Y), [SEMICOLON], exprSet(Z), [CLOSE_PARA], [COLON], cl(T).

for(t_for(U,X,Y,C)) --> [for], id(U),
    [in], [range], [OPEN_PARA], value(X), [COLON], value(Y), [CLOSE_PARA],
    [COLON], cl(C).

exprSet(t_expr(X)) --> expr(X).
exprSet(t_assign(I,E)) --> id(I),[EQUAL], exprSet(E).

expr(t_add(X,Y)) --> expr(X), [+], term(Y).
expr(t_sub(X,Y)) --> expr(X), [-], term(Y).
expr(X) --> term(X).

term(t_div(X,Y)) --> term(X), [/], fact(Y).
term(t_mul(X,Y)) --> term(X), [*], fact(Y).
term(X) --> fact(X).

fact(t_fact(X)) --> value(X).
fact(t_fact(X)) --> [OPEN_PARA], exprSet(X), [CLOSE_PARA].

value(t_value(X)) --> id(X); int1(X) ; float(X).

int1(t_int(X)) --> [X], {integer(X)}.
float(t_float(X)) --> [X], {float(X)}.
id(t_id(I)) --> [I], {atom(I)}.

str(t_str(I)) --> [LESS_THAN],[LESS_THAN],[I], {string(I)},[GREATER_THAN],[GREATER_THAN].

bool(true) --> [true].
bool(false) --> [false].
bool(t_not(X)) --> [not], bool(X).
bool(t_and(X,Y)) --> expr(X), [and], bool(Y).
bool(t_or(X,Y)) --> expr(X), [or], bool(Y).
bool(t_boolcondition(X)) --> compare_bool(X).

compare_bool(t_equal(X, Y)) --> expr(X), [EQUALEQUAL], expr(Y).
compare_bool(t_lessthan(X,Y)) --> expr(X), [LESS_THAN], expr(Y).
compare_bool(t_greaterthan(X,Y)) --> expr(X), [GREATER_THAN], expr(Y).
compare_bool(t_lessthanequal(X,Y)) --> expr(X), [LESS_THANEQUAL], expr(Y).
compare_bool(t_greaterthanequal(X,Y)) --> expr(X), [GREATER_THANEQUAL], expr(Y).
