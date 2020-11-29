:- use_module(library(clpfd)).

/* declaring input and output files */
outputFile('./solved.txt').
inputFile('./unsolved.txt').

/*********************** global control: starting the algorithm and the reading */
run:- inputFile(IF), see(IF), outputFile(F), tell(F), findKW(puzzles), readInt(N),  write('puzzles '), write(N), nl, solveProblems(N), told, seen, !.
run:- told, seen. /* close the files */

fun :-
    write('Start Solving...'), nl,
    
    N = [
        [5, 1, 5, 5, 5, 5, 5, 1, 5, 5, 5, 1, 5],
        [5, 3, 5, 3, 5, 3, 3, 5, 5, 1, 5, 1, 1],
        [5, 5, 2, 1, 5, 5, 5, 2, 3, 5, 1, 2, 5],
        [5, 2, 5, 1, 5, 2, 5, 3, 2, 2, 5, 5, 5],
        [1, 2, 1, 5, 5, 3, 5, 2, 5, 5, 5, 1, 5],
        [5, 2, 1, 5, 1, 5, 3, 5, 5, 5, 3, 5, 5],
        [5, 5, 5, 5, 1, 3, 5, 5, 5, 1, 2, 2, 5],
        [5, 2, 1, 1, 5, 5, 3, 3, 2, 5, 5, 5, 1],
        [5, 2, 2, 5, 2, 1, 5, 5, 1, 5, 2, 2, 5],
        [5, 1, 3, 1, 2, 2, 5, 2, 2, 2, 1, 2, 5],
        [5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5]],

    slant(S, N, 12, 10),
    maplist(label, S), 
    printPuzzle(N, S).

small :-
    write('Start Solving...'), nl,
    
    N = [
    [5, 5, 5, 2, 5],
    [2, 2, 5, 5, 5],
    [5, 5, 5, 2, 2],
    [5, 2, 1, 5, 5],
    [5, 5, 5, 5, 0]],

    slant(S, N, 4, 4),
    maplist(label, S), 
    printPuzzle(N, S).

big :-
    N = [
        [5,5,5,5,5, 5,1,5,5,5, 5,5,5,5,5, 5,1,5,5,5, 5],
        [5,2,2,2,2, 2,5,3,5,5, 3,1,5,1,5, 3,2,5,1,2, 5],
        [5,5,5,5,5, 5,3,5,1,5, 3,5,3,2,5, 2,1,3,5,5, 1],
        [1,5,1,5,5, 3,1,3,5,5, 2,2,5,5,2, 2,5,5,2,5, 5],
        [1,5,2,2,3, 3,5,5,5,2, 5,1,5,2,5, 2,1,3,5,3, 1],
        [5,1,5,3,2, 5,5,5,2,5, 2,5,5,5,5, 3,5,5,2,1, 5],
        [5,3,5,5,5, 2,5,5,3,3, 2,5,5,3,1, 5,5,5,3,5, 5],
        [1,1,5,5,5, 5,5,3,2,1, 2,2,3,5,5, 5,5,2,5,2, 1],
        [5,5,2,5,5, 2,3,5,2,2, 2,5,5,2,5, 3,5,1,1,2, 5],
        [5,1,2,3,5, 1,5,2,5,5, 5,2,5,5,3, 5,5,3,5,5, 1],
        [5,5,3,5,3, 1,3,2,5,2, 2,5,3,5,5, 2,5,5,3,2, 5],
        [5,3,5,5,2, 5,5,5,5,2, 2,3,1,5,5, 2,2,2,1,2, 5],
        [5,5,3,1,5, 2,5,2,5,5, 5,3,2,5,5, 1,2,5,2,5, 1],
        [5,5,5,5,1, 5,1,5,3,2, 5,5,5,3,5, 1,2,1,2,3, 5],
        [5,5,2,1,5, 5,5,2,1,1, 5,5,5,3,2, 5,2,5,5,5, 5],
        [5,2,2,5,2, 3,2,5,3,2, 5,5,3,5,5, 5,1,1,2,2, 5],
        [5,2,5,5,1, 1,5,2,5,2, 1,3,5,2,5, 5,5,2,5,1, 5],
        [5,5,1,2,5, 2,5,1,2,5, 5,1,2,5,2, 5,1,2,1,5, 5],
        [5,5,2,2,5, 1,2,2,5,5, 2,2,3,2,1, 5,2,5,2,1, 5],
        [5,3,1,5,2, 5,2,3,1,5, 2,2,2,5,2, 2,2,5,5,2, 0],
        [5,1,5,5,5, 5,5,5,1,5, 5,5,5,5,5, 1,5,1,5,5, 5]],
    slant(S, N, 20, 20),
    maplist(label, S), 
    maplist(portray_clause, S).

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
row([], _, _, _, _).
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
