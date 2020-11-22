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

    Indcies #= N2 * M2,
    length(LoopTable, Indcies),                                       
    append(LoopTable, Ls), Ls ins 0..Indcies,          

    /*add the [-1] list to the begining and end of the Slants*/
    append(Slants, Border, Temp),
    append(Border, Temp, D),
    column(Numbers, D, 0, N2, LoopTable).


column([], _, _, _, _).
column([N | Nr], [S1, S2 | Sr], Index, Width, Looptable) :-
    padd(S1, T),padd(S2, B),
    append([S2], Sr, Next),
    numbers(N, T, B, Index, Looptable, Width),
    I #= Index + Width,
    column(Nr, Next, I, Width, Looptable).

numbers([], _, _, _, _, _).
numbers([N | Nr], [_, T2 | Tr], [_, B2 | Br], Index, LoopTable, Width):-
    N #= 5,
    /*write(N), write(' : '), write(Index), nl,*/
    append([T2], Tr, NextT),  append([B2], Br, NextB),
    I #= Index +1,    
    numbers(Nr, NextT, NextB, I, Looptable, Width).

numbers([N | Nr], [T1, T2 | Tr], [B1, B2 | Br], Index, LoopTable, Width) :-
    matcher([T1, T2, B1, B2], [1, 0, 0, 1], Num),
    N #= Num,
    /*write(N), write(' : '), write(Index), nl,*/
    set_loopTable(LoopTable, Width, Index, [T1, T2, B1, B2]),
    append([T2], Tr, NextT),  append([B2], Br, NextB),    
    I #= Index +1,    
    numbers(Nr, NextT, NextB, I, Looptable, Width).


/*if we have four neighbours*/
set_loopTable(Looptable, Width, Index, Points):-
    nth0(Index, LoopTable, Pivot),
    nth0((Index - Width - 1), LoopTable, Tl),


    (nth0(0, Points, P1), P1 #= 1 -> Tl #= 1).

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