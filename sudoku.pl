:- use_module(library(clpfd)).

fun :-
    write('test'), nl,
    
    N = [
    [5, 5, 5, 2, 5],
    [2, 2, 5, 5, 5],
    [5, 5, 5, 2, 2],
    [5, 2, 1, 5, 5],
    [5, 5, 5, 5, 0]],

    slant(S, N, 4, 4),
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

    /*add the [-1] list to the begining and end of the Slants*/
    append(Slants, Border, Temp),
    append(Border, Temp, D),
    column(Numbers, D),

    Indcies #= N2 * M2,
    build_looptable(Slants, Indcies, LoopTable, N2),
    label(LoopTable),
    write(LoopTable), nl.


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
build_looptable(Slants, Length, Table, Width):-
    /*the looptable is a 1d array with as many elements as we have numbers, values can go from 1 to number of elements*/
    length(Table, Length),                                       
    Table ins 0..Length, 
    set_table_colum(Slants, Table, 0, Width).

    /*(nth0(0, Points, P1), P1 #= 1 -> Tl #= 1).*/

set_table_colum([],_, _, _).
set_table_colum([S | Sr], Table, Index, Width):-
    set_table_value(S, Table, Index, Width),
    I #= Index + Width-1,
    set_table_colum(Sr, Table, I, Width).

set_table_value([], _, _ ,_).
set_table_value([S | Sr], Table, Index, Width):-
    I0 #= Index, I1 #= Index +1, I2 #= Index + Width, I3 #= Index + Width +1,
    nth0(I0, Table, T0), nth0(I1, Table, T1), nth0(I2, Table, T2), nth0(I3, Table, T3),
    connect(S, T0, T1, T2, T3, O0, O1, O2, O3),
    I #= Index + 1,
    set_table_value(Sr, Table, I, Width).


connect(V, Tl, _, _, Br, O1, _, _, O4):-
    V #= 0,             /*if this is a '\', connect top left and bottom right*/
    loop_check(Tl, Br, O1, O4).

connect(V, _, Tr, Bl, _, _, O2, O3, _):-
    V #= 1,             /*if this is a '/', connect top right and bottom left*/
    loop_check(Tr, Bl, O2, O3).

loop_check(N, M, N2, M2):-
    N #\= M,                        /*N can not be equal to M*/

    /* if N is less then M, then we want to use N */
    N #< M,
    N2 #= N, M2 #=N.

    
loop_check(N, M, N2, M2):-
    N #\= M,                        /*N can not be equal to M*/

    /* if N is less then M, then we want to use M */
    M #< N,
    N2 #= M, M2 #=M. 

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

/*
?- nth0(2,[a,b,c],X).
X = c.
*/

/*
    *A slant board is N x M in size
        - minimum size is 1x1 (one slant, four numbers)
        - A board is considerd completed when:
            * all slants have a value
            * all numbers have fufiled thier requiermetns (X num of slant 'point' to them, no more no less)
            * no slant can be placed in such a way that it would create a loop

        - a board with a size NxM has (N+1)x(M+1) numbers
        - one slat always has four neighbours (numbers)
        - number are in the range 0-4 and _ which means it's free (can have any number of slant point to it)


        3x3 board
            * jagged array
                every other row is N+1 long
                total length (rows) 2M+1
        [
        [_, 0, 1, 1]
        [x, x, x]
        [_, 0, 1, 1]
        [x, x, x]
        [_, 0, 1, 1]
        ]

    **Implementation idea:
        1. make sure the size is correct
        2. make sure that the number cell have a value between -1 -> 4 (where -1 is a 'free' cell)
        3. make sure that each slant has a valid value 0 -> 1 ('/'' or '\'')
        4. looking at a cell (number) make sure that it does not have more neighbours (slants) pointing to it then is allowed
        5. make sure no slant is set in a way that would create a loop
*/