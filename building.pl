:- use_module(library(clpfd)).
outputFile('./puzzle_solved.txt ').
inputFile('./puzzle_unsolved.txt').

/********************* solving the puzzle */
doSolve(P,P).
:- use_module(library(clpfd)).
list(N, Ls) :-
    length(Ls, N).

slant(Slants, Numbers, M, N) :-
    /* 1. make sure the size is correct*/
    N #> 0 , M #> 0,                              /* slant board must be 1x1 or bigger */
    N2 #= N+1, M2 #= M+1,

    /* 2. make sure that each slant has a valid value 0 -> 1 ('/'' or '\'') */
    length(Slants, N),                          /* length of colums is N */
    maplist(list(M), Slants),                   /* length of rows is M */
    append(Slants, Ss), Ss ins 0..1,            /* a slant must have a value of 0 or 1 ('/ or '\') */

    /* 3. make sure that the number cell have a value between -1 -> 4 (where -1 is a 'free' cell) */
    length(Numbers, N2),                        /* length of colums is N */
    maplist(list(M2), Numbers),                 /* length of rows is M */
    append(Numbers, Vs), Vs ins 0..5,           /* a slant must have a value of 0 or 1 ('/ or '\') */

    /* 4. looking at a cell (number) make sure that it does not have more neighbours (slants) pointing to it then is allowed */
    length(Border, 1),                  /*create a list of [-1, -1, -1...., M]*/
    maplist(list(M), Border), 
    append(Border, Bs), 
    Bs ins -1,       

    /*add the [-1] list to the begining and end of the Slants*/
    append(Slants, Border, Temp),
    append(Border, Temp, D),
    column(Numbers, D),

    Indcies #= N2 * M2,
    build_looptable(Slants, Indcies, M2).


/*Make sure there is a valid number of slants pointing to a given number*/
/*----------------------------------------------------------------------*/
column([], _).
column([N | Nr], [S1, S2 | Sr]) :-
    padd(S1, T),padd(S2, B),
    append([S2], Sr, Next),
    numbers(N, T, B),
    column(Nr, Next).

numbers([], _, _).
numbers([N | Nr], [_, T2 | Tr], [_, B2 | Br]):-
    N #= 5,
    append([T2], Tr, NextT),  append([B2], Br, NextB),   
    numbers(Nr, NextT, NextB).

numbers([N | Nr], [T1, T2 | Tr], [B1, B2 | Br]) :-
    matcher([T1, T2, B1, B2], [1, 0, 0, 1], Num),
    N #= Num,
    append([T2], Tr, NextT),  append([B2], Br, NextB),      
    numbers(Nr, NextT, NextB).


/*Make sure there loops given a set of slants*/
/*-------------------------------------------*/
build_looptable(Slants, Length, Width):-
    /*the looptable is a 1d array with as many elements as we have numbers, values can go from 1 to number of elements*/
    length(Table, Length), Table ins 1..Length, all_distinct(Table), ordered(Table),         
    set_table_row(Slants, Table, 0, Width).


set_table_row([], _, _, _).
set_table_row([S | Sr], Table, Index, Width):-
    label(Table),
   /* write('Table: '), write(Table), nl,*/
    set_table_value(S, Table, UpdatedTable, Index, Width),
    I #= Index + Width,
    set_table_row(Sr, UpdatedTable, I, Width).

set_table_value([], T, T, _, _).
set_table_value([S | Sr], Table, UpdatedTable, Index, Width):-
    
    /*find the indecies of the four numbers connectet to this slant S, I0... I3*/
    I0 #= Index, I1 #= Index +1, I2 #= Index + Width, I3 #= Index + Width +1,
/*
    write(I0), write( ' - '), write(I1), nl,
    write(I2), write( ' - '), write(I3), nl, nl,
*/
    /*find the find the value for those indecies in our looptable, T0... T3*/
    nth0(I0, Table, T0), nth0(I1, Table, T1), nth0(I2, Table, T2), nth0(I3, Table, T3),

    connect(S, T0, T1, T2, T3, I0, I1, I2, I3, Table, Updated),

    I #= Index + 1,
    set_table_value(Sr, Updated, UpdatedTable, I, Width).

 /* a slant must have a value of 0 or 1 ('/ or '\') */


 connect(V, _, Tr, Bl, _, _, I1, I2, _, StartTable, UpdatedTable):-
    V #= 0,                         /*if this is a '/', connect top right and bottom left*/
    Tr #\= Bl,
    loop_check(Tr, Bl, I),
    replace(StartTable, I1, I, U),
    replace(U, I2, I, UpdatedTable).

connect(V, Tl, _, _, Br, I0, _, _, I3, StartTable, UpdatedTable):-
    V #= 1,                         /*if this is a '\', connect top left and bottom right*/
    Tl #\= Br,
    loop_check(Tl, Br, I),
    replace(StartTable, I0, I, U),
    replace(U, I3, I, UpdatedTable).



loop_check(N, M, I):-
    N #< M, I #= N.                 /* if N is less then M, then we want to use N */
    
loop_check(N, M, I):-
    M #< N, I #= M.                 /*if M is less then N, then we want to use M */


/*-------------------*/
/*Helpers*/

matcher([],[],0).
matcher([E | List], [P | Pattern], Num):-
    E #\= P,
    matcher(List, Pattern, Num).
matcher([E | List], [P | Pattern], Num):-
    E #= P,
    matcher(List, Pattern, R),
    Num #= R +1.

padd(List, Padded) :-
    append(List, [-1], Temp),
    append([-1], Temp, Padded).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- NI #= I-1, replace(T, NI, X, R).

ordered([]).
ordered([_]).
ordered([X,Y|Xs]) :- X #=< Y, ordered([Y|Xs]).

empty([]).





/********************** reading the input */

readProblem(puzzle(X,Y),List):- 
findKW(size), readInt(X), readInt(Y), NyY is Y +1,length(List,NyY), readLines(X,List), nl.
   

findKW(KW):- string_codes(KW,[H|T]), peek_code(H), readKW([H|T]),  !.
findKW(_):- peek_code(-1), !, fail.
findKW(KW):- get_code(_), findKW(KW).


readKW([]):- get_code(_).
readKW([H|T]):- get_code(H), readKW(T).

readHintLine(0,[E]):- get_code(C),translate(C,E),get_code(_).
readHintLine(N,[E|R]):- N>0, N1 is N-1, get_code(I), translate(I,E), get_code(_), readHintLine(N1,R).

readXLine(0):- get_code(_).
readXLine(N):- N>0, N1 is N-1, findKW('x'), readXLine(N1) .

readLines(_,[]).
readLines(N,[H]):- readHintLine(N,H).
readLines(N,[H|T]):- readHintLine(N,H), readXLine(N), readLines(N,T).

readInt(N):- get_code(M), handleCode(M,N).

handleCode(M,N):- is_number_code(M,N1), !, continueInt(N1,N).
handleCode(-1,_):- !, fail. /* EOF */
handleCode(_,N):- readInt(N).

continueInt(O,N):- get_code(M), is_number_code(M,M1), !, H is 10*O+M1, continueInt(H,N).
continueInt(N,N).

translate(95, 5).
translate(32, ' ').
translate(48, 0).
translate(49, 1).
translate(50, 2).
translate(51, 3).
translate(52, 4).
translate(120,'x').
translate(X, X).


is_number_code(N, N1):- N>=48, N<58, N1 is N-48.
is_number_code(95,0).

/*********************** global control: starting the algorithm and the reading */
% N is number of puzzles
% F is the solved file path
% IF is the unsolved file path
% S is 'puzzle(7,5)puzzle(4,4)'
% N is the puzzle size, 44, 75


% add the puzzle in the unsolved_puzzle.txt, then write run. in the terminal
run:- inputFile(IF), see(IF), outputFile(F), tell(F), findKW(puzzles), readInt(N),  write('puzzles '), write(N), nl, solvePuzzles(N), told, seen, !.
run:- told, seen. /* close the files */
solvePuzzles(0).
solvePuzzles(N):- N>0, readProblem(_,X),
 length(X, SizeY),
  nth0(0, X, X1), length(X1, SizeX), NewSizeX is SizeX -1, NewSizeY is SizeY -1,
  write(NewSizeX), write(', '), write(NewSizeY), nl,   maplist(label,X), maplist(portray_clause, X),nl,

  slant(S22,X,NewSizeX,NewSizeY),
  maplist(label,S22),
  maplist(portray_clause, S22).
 
  !,  N1 is N-1, solvePuzzles(N1).