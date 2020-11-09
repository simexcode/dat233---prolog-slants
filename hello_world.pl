
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

/*
:- initialization main, halt.
*/

main :- 
    write('Hello, World!'), nl.

slant(Slants) :-
    N > 0 , M > 0,                          /* slant board must be 1x1 or bigger */
    length(Slants, N),                      /* length of colums is N */
    maplist(M, Slants),                     /* length of rows is M */
    append(Slants, Ss), Ss ins 1..2.        /* a slant must have a value of 0 or 1 ('/ or '\') */
