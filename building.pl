:- use_module(library(clpfd)).
outputFile('./puzzle_solved.txt ').
inputFile('./puzzle_unsolved.txt').

/********************* solving the puzzle */
doSolve(P,P).

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

    Indcies #= N2 * M2,
    /*the looptable is a 1d array with as many elements as we have numbers, values can go from 1 to number of elements*/
    length(Table, Indcies), Table ins 1..Indcies, all_distinct(Table), ordered(Table),     
    label(Table),
    Sw #= M2 * -1,
    /*add the [-1] list to the begining and end of the Slants*/
    append(Slants, Border, Temp),
    append(Border, Temp, D),
    row(Numbers, D, Sw, M2, Table).


/*Make sure there is a valid number of slants pointing to a given number*/
/*----------------------------------------------------------------------*/
row([], _, _, _, _):- !.
row([N | Nr], [S1, S2 | Sr], Index, Width, Table) :-
    
    padd(S1, T), padd(S2, B),
    numbers(N, T, B),               /*find what slant values work for row T, B, given the row numbers N*/
   
    loop_values(S1, Index, Width, Table, U),
    I #= Index + Width,

    row(Nr, [S2 | Sr], I, Width, U).

numbers([], _, _).
numbers([N | Nr], [_, T2 | Tr], [_, B2 | Br]):-
    N #= 5, 
    numbers(Nr, [T2| Tr], [B2 | Br]).

numbers([N | Nr], [T1, T2 | Tr], [B1, B2 | Br]) :-
    matcher([T1, T2, B1, B2], [1, 0, 0, 1], Num),
    N #= Num,
    numbers(Nr, [T2| Tr], [B2 | Br]).

loop_values([], _, _, T, T).
loop_values([S | Sr], Index, Width, Table, U):- S #= (-1), loop_values(Sr, Index, Width, Table, U).
loop_values([S | Sr], Index, Width, Table, U):- 
    
    S #= 0, I0 #= Index +1, I1 #= Index + Width, nth0(I0, Table, T0), nth0(I1, Table, T1),

    T0 #\= T1, 
    smallest(T0, T1, I, O), 
    replaceP(O, I, Table, UpdatedTable), 
    Index1 #= Index + 1, 
    loop_values(Sr, Index1, Width, UpdatedTable, U).

loop_values([S | Sr], Index, Width, Table, U):- 
    S #= 1, I0 #= Index, I1 #= Index + Width +1, nth0(I0, Table, T0), nth0(I1, Table, T1),
    T0 #\= T1, smallest(T0, T1, I, O), replaceP(O, I, Table, UpdatedTable), Index1 #= Index + 1 , loop_values(Sr, Index1, Width, UpdatedTable, U).

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

replaceP(_, _, [], []).
replaceP(O, R, [O|T], [R|T2]) :- !, replaceP(O, R, T, T2).
replaceP(O, R, [H|T], [H|T2]) :- H #\= O, !, replaceP(O, R, T, T2).

ordered([]).
ordered([_]).
ordered([X,Y|Xs]) :- X #=< Y, ordered([Y|Xs]).

smallest(N, M, I, O):- N #< M, I #= N, O #= M.
smallest(N, M, I, O):- M #< N, I #= M, O #= N.  

empty([]).

/*-------------------*/
/*Printing*/
printPuzzle([], []).
printPuzzle([N], []):- numberRow(N).
printPuzzle([N | Nr], [S | Sr]):- numberRow(N), nl, write(' '),   slantRow(S), nl, printPuzzle(Nr, Sr).

numberRow([]).
numberRow([N | Nr]):- N #= 5, write('_'), write(' '), !, numberRow(Nr).
numberRow([N | Nr]):- N #\= 5,write(N), write(' '), !, numberRow(Nr).

/* a slant must have a value of 0 or 1 ('/ or '\') */
slantRow([]).
slantRow([S | Sr]):- S #= 1, write('\\'), write(' '), slantRow(Sr).
slantRow([S | Sr]):- S #= 0, write('/'), write(' '), slantRow(Sr).


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
run:- inputFile(IF), see(IF), outputFile(F), tell(F), findKW(puzzles), readInt(N),  write('puzzles '), write(N), solvePuzzles(N), told, seen, !.
run:- told, seen. /* close the files */
solvePuzzles(0).
solvePuzzles(N):- N>0, readProblem(_, Numbers),
  length(Numbers, SizeY), nth0(0, Numbers, X1), length(X1, SizeX), NewSizeX is SizeX -1, NewSizeY is SizeY -1,
  write('size'), write(NewSizeX), write('x'), write(NewSizeY), nl,

  slant(Slants, Numbers, NewSizeX, NewSizeY), maplist(label, Slants), printPuzzle(Numbers, Slants),
  !,  N1 is N-1, solvePuzzles(N1).