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
    length(LoopTable, N2),                        
    maplist(list(M2), LoopTable),                 
    append(LoopTable, Ls), Ls ins 0..Indcies,          

    /*add the [-1] list to the begining and end of the Slants*/
    append(Slants, Border, Temp),
    append(Border, Temp, D),
    column(Numbers, D).


column([],_).
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