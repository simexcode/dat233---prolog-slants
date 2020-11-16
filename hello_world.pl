

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
:- use_module(library(clpfd)).
:- use_module(library(apply)).
:- use_module(library(lists)).


list(N, Ls) :-
    length(Ls, N).

matcher([],[],0).
matcher([E | List], [P | Pattern], Num):-
    E #\= P,
    matcher(List, Pattern, Num).
matcher([E | List], [P | Pattern], Num):-
    E #= P,
    matcher(List, Pattern, R),
    Num #= R +1.

list_sum([Item], Item).
list_sum([Item1,Item2 | Tail], Total) :-
    list_sum([Item1+Item2|Tail], Total).

equals(X,Y,Z,Y) :-
    (X #= Y -> Y  Z+1; Y #= 0).

puzzle(Slants, Numbers, N, M) :-
    /* 1. make sure the size is correct*/
    N > 0 , M > 0,                              /* slant board must be 1x1 or bigger */
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
    length(Border, 1),
    maplist(list(M), Border), 
    append(Border, Bs), 
    Bs ins -1,

    append(Slants, Border, Temp),
    append(Border, Temp, D).

    column(Numbers, D).


column([],_).
column([N | Nr], [S1, S2 | Sr]) :-
    append(S1, -1, Temp),
    append(-1, Temp, D1),
    append(S2, -1, Temp2),
    append(-1, Temp2, D2),
    numbers(N, D1, D2),
    column(Nr, [S2, Sr]).

numbers([], _, _).
numbers([N | Nr], [_, T2 | Tr], [_, B2 | Br]):-
    N #= 5,
    numbers(Nr, [T2, Tr], [B2, Br]).

numbers([N | Nr], [T1, T2 | Tr], [B1, B2 | Br]) :-
    matcher([T1, T2, B1, B2], [1, 0, 0, 1], Num),
    N #= Num,    
    numbers(Nr, [T2, Tr], [B2, Br]).
    