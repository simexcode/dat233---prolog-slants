/********************* declaring input and output files */
outputFile('./battleships_solved.txt').
inputFile('./battleships_unsolved.txt').

/********************* solution algorithm - top level */
doSolve(battleships(size(N),ships(S),horizontal(H),vertical(V),grid(G)),Solution):-
  prepareProblem(N,S,H,V,G,Problem), % create a better problem structure for the solution process
  prepareCode(N),                    % create adapted facts for quicker solution
  applyHints(Problem, Solution),     % get as much as possible information out of the hints
  simpleSolve(Solution,-1,0),        % use simple rules for straightforward solving
  placeShips(Solution,Solution).     % place ships and make sure all constraints are met

simpleSolve(_,N,N).                  % stop simple solving when no changes are detected
simpleSolve(P,N1,N2):- N1<N2,        % continue simple solving when there are changes
  P=problem(Lines,Blocks,_),         % extract lines and blocks
  checkLines(Lines),                 % check the lines and set values if possible
  checkBlocks(Blocks),               % check the blocks and set values if possible
  countNonVars(Blocks,N3),           % count how many slots are already solved
  simpleSolve(P,N2,N3).              % start recursively.

/********************* solution algorithm - just including the hints and removing special ship parts */
/* applyHints: applying lines hints and block hints */
applyHints(problem(Lines,Blocks,Ships),problem(HLines,NBlocks,Ships)):-
  applyLinesHints(Lines,HLines), applyBlocksHints(Blocks,NBlocks).

/* applyLinesHints: apply hints for each line */
applyLinesHints([],[]).
applyLinesHints([H|T],[H1|T1]):- applyOneLineHints(H,H1), applyLinesHints(T,T1).

/* applyOneLineHints: translate each element */
applyOneLineHints([],[]).
applyOneLineHints([H|T],[H1|T1]):- lTrans(H,H1), !, applyOneLineHints(T,T1).

/* lTrans: translate all ship types into 'S', keep everything else. */
lTrans(X,X):- var(X).
lTrans(X,X):- number(X).
lTrans('-','-').
lTrans(_,'S').

/* applyBlockHints: apply hints per block */
applyBlocksHints([],[]).
applyBlocksHints([H|T],[H1|T1]):- blockHint(H,H1), applyBlocksHints(T,T1).

/* blockHint: handling a hint, translating ships into 'S' and filling more info. Note the handling of '+'. */
blockHint([A,B,C,D,E,F,G,H,I],[A1,B1,C1,D1,E,F1,G1,H1,I1]):- var(E), !, bTrans([A,B,C,D,F,G,H,I],[A1,B1,C1,D1,F1,G1,H1,I1]).
blockHint([A,B,C,D,'-',F,G,H,I],[A1,B1,C1,D1,'-',F1,G1,H1,I1]):- bTrans([A,B,C,D,F,G,H,I],[A1,B1,C1,D1,F1,G1,H1,I1]).
blockHint([A,B,C,D,'S',F,G,H,I],['-',B1,'-',D1,'S',F1,'-',H1,'-']):- bTrans([A,B,C,D,F,G,H,I],['-',B1,'-',D1,F1,'-',H1,'-']).
blockHint([A,B,C,D,'A',F,G,H,I],['-','-','-','-','S','-','-','S','-']):- bTrans([A,B,C,D,F,G,H,I],['-','-','-','-','-','-','S','-']).
blockHint([A,B,C,D,'V',F,G,H,I],['-','S','-','-','S','-','-','-','-']):- bTrans([A,B,C,D,F,G,H,I],['-','S','-','-','-','-','-','-']).
blockHint([A,B,C,D,'<',F,G,H,I],['-','-','-','-','S','S','-','-','-']):- bTrans([A,B,C,D,F,G,H,I],['-','-','-','-','S','-','-','-']).
blockHint([A,B,C,D,'>',F,G,H,I],['-','-','-','S','S','-','-','-','-']):- bTrans([A,B,C,D,F,G,H,I],['-','-','-','S','-','-','-','-']).
blockHint([A,B,C,D,'+',F,G,H,I],['-','S','-','-','S','-','-','S','-']):- bTrans([A,B,C,D,F,G,H,I],['-','S','-','-','-','-','S','-']).
blockHint([A,B,C,D,'+',F,G,H,I],['-','-','-','S','S','S','-','-','-']):- bTrans([A,B,C,D,F,G,H,I],['-','-','-','S','S','-','-','-']).
blockHint([A,B,C,D,'*',F,G,H,I],['-','-','-','-','S','-','-','-','-']):- bTrans([A,B,C,D,F,G,H,I],['-','-','-','-','-','-','-','-']).

/* bTrans and bT: translating ships to 'S' */
bTrans([],[]).
bTrans([H|T],[H1|T1]):- bT(H,H1), !, bTrans(T,T1).
bT(X,X):- var(X).
bT('-','-').
bT('S','S').
bT('A','S').
bT('V','S').
bT('<','S').
bT('>','S').
bT('+','S').
bT('*','S').
bT(X,X).

/********************* solution algorithm - checking hints and rules */
/* checkLines and checkOneLine: checking lines one by one */
checkLines([]).
checkLines([H|T]):- checkOneLine(H), checkLines(T).
checkOneLine(L):- findall(L,correctBattleLine(L),[]), !, fail.   % if there are no possible options, the line is incorrect
checkOneLine(L):- findall(L,correctBattleLine(L),X), X=[L], !.   % if there is just one option left, use it
checkOneLine(_).                                                 % otherwise do not change anything

/* correctLine and countShips: How to determine when a line of a certain size is correct */
correctLine(Size,[Count|Line]):- length(Line,Size), countShips(Line,Count).
countShips([],0).
countShips(['-'|T],C):- countShips(T,C).
countShips(['S'|T],C):- countShips(T,D), C is D+1.

/* prepareCode: correct lines for the current size are pre-computed for better speed */
prepareCode(Size):- retractall(correctBattleLine(_)), correctLine(Size,L), assert(correctBattleLine(L)), fail.  
prepareCode(_).

/* checkBlocks and checkOneBlock: handling diagonals for all 3x3 blocks */
checkBlocks([]).
checkBlocks([H|T]):- checkOneBlock(H), checkBlocks(T).
checkOneBlock([A,_,C,_,E,_,G,_,I]):- nonvar(E), E='S', !, A='-', C='-', G='-', I='-'.
checkOneBlock(_).

/********************* solution algorithm - placing the ships, starting from big ones */
/* placeShips: placing the next ship, and checking validity */
placeShips(_,problem(_,_,[])).
placeShips(problem(OL,OB,_),problem(L,B,[H|T])):- 
  placeOneShip(H,L,L1,B,B1), 
  checkLines(OL), checkBlocks(OB),
  continuePlacement(H,T,OL,L1,LNew),
  placeShips(problem(OL,OB,_),problem(LNew,B1,T)).

/* continuePlacement: determine what puzzle to use for ship finding
   for each size, the ships of that size are replaced with 'X' instead of 'S' */
continuePlacement(_,[],_,_,_).             % we are done, no need to continue
continuePlacement(H,[H|_],_,L,L).          % same size as before, use same solution
continuePlacement(H,[X|_],L,_,L):- H\=X.   % different size, use fresh solution

/* placeOneShip: if size > 1, check in lines, else check in blocks */
placeOneShip(1,L,L,B,B1):- !, % ship of size 1 - requires that all variables are bound before
  append(X,[['-','-','-','-','S','-','-','-','-']|Y],B),
  append(X,[['-','-','-','-','X','-','-','-','-']|Y],B1), !.
placeOneShip(N,L,L1,B,B):- findShip(N,L,L1).

/* findShip and findShipInLine: go through all lines to find ship of a size */
findShip(N,[[C|R]|T],[[C|R1]|T]):- findShipInLine(0,N,R,R1).
findShip(N,[_|T],T1):- findShip(N,T,T1).
findShipInLine(0,N,L,L1):- isShip(N,L,L1).
findShipInLine(_,N,['-'|L],['-'|L1]):- isShip(N,L,L1).
findShipInLine(_,N,[H|L],[H|L1]):- findShipInLine(1,N,L,L1).

/* isShip: mark ship of size N with 'X' instead of 'S' */
isShip(0,[],[]).
isShip(0,['-'|R],['-'|R]).
isShip(N,['S'|R],['X'|R1]):- N>0, N1 is N-1, isShip(N1,R,R1).

/********************* restructuring the problem representation */
/* prepareProblem: add columns, outer lines, and blocks, sort the ships according to size */
prepareProblem(S,Ships,H,V,G,problem(Lines,Blocks,NShips)):-
  createLines(V,G,HLines),
  transpose(G,VG),
  createLines(H,VG,VLines),
  append(HLines,VLines,Lines),
  S2 is S+2, addOuterLines(S2,G,GG),
  extractBlocks(GG,Blocks),
  msort(Ships,HShips), reverse(HShips,NShips).

/* createLines: add hints to the grid lines */
createLines([],[],[]).
createLines([H|T],[L|LL],[[H|L]|Lines]):- createLines(T,LL,Lines).

/* addOuterLines: add an empty line on all sides of the board */
addOuterLines(S,G,[L|GG]):- length(L,S), moreOuterLines(S,G,GG).
moreOuterLines(S,[],[L]):- length(L,S).
moreOuterLines(S,[L|LL],[[_|X]|XX]):- append(L,[_],X), moreOuterLines(S,LL,XX).

/* extractBlocks and blocks: extract 3x3 blocks from the puzzle */
extractBlocks([_,_],[]).
extractBlocks([A,B,C|T],Blocks):- blocks(A,B,C,BL), extractBlocks([B,C|T],BB), append(BL,BB,Blocks).
blocks([_,_],[_,_],[_,_],[]).
blocks([A1,B1,C1|T1],[A2,B2,C2|T2],[A3,B3,C3|T3],[[A1,B1,C1,A2,B2,C2,A3,B3,C3]|T4]):-
  blocks([B1,C1|T1],[B2,C2|T2],[B3,C3|T3],T4).

/********************* auxiliary */
/* transpose and firstCol: transpose the grid */
transpose([], []).
transpose([[H|T] |Tail], [[H|NT] |NTail]) :- 
	firstCol(Tail, NT, Rest), transpose(Rest, NRest), firstCol(NTail, T, NRest).
firstCol([], [], []).
firstCol([[H|T] |Tail], [H|Col], [T|Rows]) :- firstCol(Tail, Col, Rows).

/* countNonVars: find how many slots are already solved */
countNonVars([],0).
countNonVars([[_,_,_,_,V,_,_,_,_]|T],N1):- nonvar(V), !, countNonVars(T,N), N1 is N+1.
countNonVars([_|T],N):- countNonVars(T,N).

/********************* writing the result */
writeFullOutput(problem(L,_,_)):- 
  append(Lines,Columns,L), length(Lines,N), length(Columns,N), 
  write('size '), write(N), write('x'), write(N), nl, writeGrid(Lines).

writeGrid([]).
writeGrid([H|T]):- writeGridLine(H), writeGrid(T).

writeGridLine([]):- nl.
writeGridLine([H|T]):- writeElement(H), writeGridLine(T).

writeElement(X):- number(X), !.
writeElement(X):- var(X), write('_ ').
writeElement(X):- nonvar(X), write(X), write(' ').

/********************** reading the input */
readProblem(battleships(size(N),ships(S),horizontal(H),vertical(V),grid(Grid))):- 
  findKW(size), readInt(N), readInt(M), M=N, length(H, N), length(V,N), length(Grid,N), 
  readShips(S), readHorizontal(H), readVertical(V), findKW(hints), readGridLines(N,Grid).

findKW(KW):- string_codes(KW,[H|T]), peek_code(H), readKW([H|T]), !.
findKW(_):- peek_code(-1), !, fail.
findKW(KW):- get_code(_), findKW(KW).

readKW([]):- get_code(_).
readKW([H|T]):- get_code(H), readKW(T).

readShips(L):- findKW(ships), readShipCount(L).

readShipCount(L):- 
  peek_code(C), is_number_code(C,_), readInt(Count), readInt(Size), 
  expandShips(Count,Size,L1), readShipCount(L2), append(L1,L2,L), !.
readShipCount([]).

expandShips(0,_,[]).
expandShips(N,S,[S|T]):- N>0, N1 is N-1, expandShips(N1,S,T).

readHorizontal(L):- findKW(horizontal), readNumberLine(L).
readVertical(L):- findKW(vertical), readNumberLine(L).

readGridLines(_,[]).
readGridLines(N,[H|T]):- length(H,N), readGridLine(H), readGridLines(N,T).

readGridLine([]).
readGridLine([E|T]):- get_code(M), translate(M,E), !, readGridLine(T).

translate(-1,'ERROR: EOF').
translate(63,_).
translate(X,E):- whitespace(X), get_code(Y), translate(Y,E).
translate(X,E):- name(E,[X]).

whitespace(10). whitespace(12). whitespace(32).

readNumberLine([]).
readNumberLine([E|T]):- readInt(E), readNumberLine(T).

readInt(N):- get_code(M), handleCode(M,N).

handleCode(M,N):- is_number_code(M,N1), !, continueInt(N1,N).
handleCode(-1,_):- !, fail. /* EOF */
handleCode(_,N):- readInt(N).

continueInt(O,N):- get_code(M), is_number_code(M,M1), !, H is 10*O+M1, continueInt(H,N).
continueInt(N,N).

is_number_code(N, N1):- N>=48, N<58, N1 is N-48.

/*********************** global control: starting the algorithm and the reading */
run:- inputFile(IF), see(IF), outputFile(F), tell(F), findKW(puzzles), readInt(N),  write('puzzles '), write(N), nl, solveProblems(N), told, seen, !.
run:- told, seen. /* close the files */

solveProblems(0).
solveProblems(N):- N>0, readProblem(P), doSolve(P, S), writeFullOutput(S), !, N1 is N-1, solveProblems(N1).
%solveProblems(N):- N>0, readProblem(P), findall(S,doSolve(P, S),L), writeAllSolutions(L), !, N1 is N-1, solveProblems(N1).

writeAllSolutions([]):- write('no more solutions.'), nl.
writeAllSolutions([H|T]):- write('Here is a solution:'), nl, writeFullOutput(H), writeAllSolutions(T).

:- nl,nl,write(' try running "?- run."'), nl,nl,nl.

:- run.
:- halt.
