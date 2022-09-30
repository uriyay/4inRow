/*
initial cell: 0
player cell: 1
computer cell: 2
*/

init(Board) :-
    %Board = (MinRowId, MinColId, MaxColId, InnerBoard)
    % 7 columns and 6 rows
    %At first MinColId = 7 and MaxColId = 1, later updateBoard will set them correctly
    Board = (6, 7, 1, [
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0]
    ]).

getInnerBoard((_MinRowId, _MinColId, _MaxColId, InnerBoard), InnerBoard).

displayLine([]) :- 
    !, format("\n").

displayLine([X|Rest]) :-
    ((X = 0, !, D = " ");
    (X = 1, !, D = "O");
    (X = 2, !, D = "#")),
    format("[~w]", D),
    displayLine(Rest).

displayLines([]) :- !.
displayLines([Line|Rest]) :-
    displayLine(Line),
    displayLines(Rest).

displayBoard(Board) :-
    getInnerBoard(Board, InnerBoard),
    displayLines(InnerBoard),
    format("---------------------\n"),
    format("[1][2][3][4][5][6][7]\n").

/* getAnswer(+Answer) - reads one-char answer and returns it as an atomic (for example: "1" -> 1) */
getAnswer(Answer) :-
    format("> "),
    get(AnswerAscii),
    name(Answer, [AnswerAscii]).

calcRow0(Col, Board, CurRow, Row) :-
    (CurRow < 0, !, Row = -1);
    (nth1(CurRow, Board, RowElement),
    nth1(Col, RowElement, Cell),
    %if Cell = 0 then return Row = CurRow
    (Cell = 0, !, Row = CurRow);
    %else if calcRow0 returns a valid row - return it
    (CurRow1 is CurRow - 1, !, calcRow0(Col, Board, CurRow1, Row));
    %else - return -1
    (Row = -1)
    ).

calcRow(Col, Board, Row) :-
    calcRow0(Col, Board, 6, Row).

getCol(Col) :-
    format("Where do you want to place your token? (1-7)\n"),
    getAnswer(Col1),
    (number(Col1), 1 =< Col1, Col1 =< 7, Col = Col1);
    (format("Error: invalid answer\n"), getCol(Col)).

updateBoard(Board, Col, Row, Value, UpdatedBoard) :-
    Board = (MinRowId, MinColId, MaxColId, InnerBoard),
    %get RowElement at Row index
    nth1(Row, InnerBoard, RowElement, RestRows),
    %get the row without the cell
    nth1(Col, RowElement, _Cell, RowElementRest),
    %build a new row from RowElementRest where RowElement1[Col] = Value
    nth1(Col, RowElement1, Value, RowElementRest),
    %build a new UpdatedBoard which at Row there is RowElement1
    nth1(Row, UpdatedInnerBoard, RowElement1, RestRows),
    ((Row < MinRowId, !, MinRowId1 = Row);
    (MinRowId1 = MinRowId)),
    ((Col < MinColId, !, MinColId1 = Col);
    (MinColId1 = MinColId)),
    ((Col > MaxColId, !, MaxColId1 = Col);
    (MaxColId1 = MaxColId)),
    UpdatedBoard = (MinRowId1, MinColId1, MaxColId1, UpdatedInnerBoard).

getCell(Board, Row, Col, Value) :-
    nth1(Row, Board, RowElement),
    nth1(Col, RowElement, Value).

/****  The alpha-beta algorithm ***/
/*
Pos = pos(Board, Player)
where Player can be computer or player
Since the player always play first, then he will be Max and the computer will be Min
*/
max_to_move(pos(_, player)).
min_to_move(pos(_, computer)).

% moves(-Pos, +PosList) :- for given Pos - retrieve all the possible positions
moves(Pos, PosList) :-
    setof(Pos1, move(Pos, Pos1), PosList).

move(Pos, Pos1) :-
    Pos = pos(Board, Player),
    getInnerBoard(Board, InnerBoard),
    % when choosing a column it doesn't matter who the player is
    between(1, 7, ColId1),
    calcRow(ColId1, InnerBoard, RowId1),
    RowId1 \= -1,
    ((Player = player, Player1 = computer, Val = 1);
    (Player = computer, Player1 = player, Val = 2)),
    updateBoard(Board, ColId1, RowId1, Val, UpdatedBoard),
    Pos1 = pos(UpdatedBoard, Player1).

staticval(Pos, Val) :-
    Pos = pos(Board, Player),
    (((won0(Board, Winner),
    nonvar(Winner)),
    ((Winner = player, Val is 1);
    (Winner = computer, Val is -1)));
    Val is 0).
    %for now - just count the disks of player
    %count_disks(Board, player, Val).

count_disks(Board, Player, Val) :-
    ((Player = player, !, Elem = 1);
    (Player = computer, !, Elem = 2)),
    count_disks0(Board, Elem, Val).

count_disks0([], Elem, 0).

count_disks0([Row|Rest], Elem, Val) :-
    count_disks0(Rest, Elem, Val1),
    count_disks_in_row(Row, Elem, Val2),
    Val is Val1 + Val2.

count_disks_in_row([], Elem, 0).
count_disks_in_row([X|Rest], Elem, Val) :-
    count_disks_in_row(Rest, Elem, Val1),
    ((X = Elem, !, Val is Val1 + 1);
    (Val is Val1)).

alphabeta(Pos, Alpha, Beta, GoodPos, Val, MaxDepth) :- 
    Beta is 1,
    Alpha is -Beta,
    alphabeta0(Pos, Alpha, Beta, GoodPos, Val, 1, MaxDepth).

alphabeta0(Pos, Alpha, Beta, GoodPos, Val, CurDepth, MaxDepth) :- 
    staticval(Pos, Val1), % Static value Of Pos 
    (
    % if max depth exceeded or that Pos is a winning state - return Val
    ((CurDepth > MaxDepth;
    Val1 = 1;
    Val1 = -1),
    Val = Val1,
    GoodPos = Pos);
    % else - evaluate the children of Pos
    (moves(Pos, PosList), !, 
    CurDepth1 is CurDepth + 1,
    boundedbest(PosList, Alpha, Beta, GoodPos, Val, CurDepth1, MaxDepth))).
    
boundedbest([Pos | PosList], Alpha, Beta, GoodPos, GoodVal, CurDepth, MaxDepth) :-
    alphabeta0(Pos, Alpha, Beta, _, Val, CurDepth, MaxDepth), 
    goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, CurDepth, MaxDepth).

goodenough([], _ , _ , Pos, Val, Pos, Val, CurDepth, MaxDepth) :- !. % No Other candidate 
    
goodenough( _, Alpha, Beta, Pos, Val, Pos, Val, CurDepth, MaxDepth) :-
    min_to_move(Pos), Val > Beta, !; % Maximizer attained upper bound 
    max_to_move(Pos), Val < Alpha, !. % Minimizer attained lower bound

goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, CurDepth, MaxDepth) :-
    newbounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta), 
    boundedbest(PosList, NewAlpha, NewBeta, Pos1, Val1, CurDepth, MaxDepth), 
    betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal). 

newbounds(Alpha, Beta, Pos, Val, Val, Beta) :- 
    min_to_move(Pos), Val > Alpha, !. % Maximizer increased lower bound 

newbounds(Alpha, Beta, Pos, Val, Alpha, Val) :- 
    max_to_move( Pos), Val < Beta, !. % Minimizer decreased upper bound 

newbounds(Alpha, Beta, _, _, Alpha, Beta). % Otherwise bounds unchanged 

betterof(Pos, Val, Pos1, Val1, Pos, Val) :- % Pos better than Pos1
    min_to_move(Pos), Val > Val1, !; 
    max_to_move(Pos), Val < Val1, !.

betterof(_, _ , Pos1, Val1, Pos1, Val1). % Otherwise Pos1 better

/******/

playUser(Board, UpdatedBoard) :-
    getInnerBoard(Board, InnerBoard),
    format("Player turn!\n-------------\n"),
    getCol(Col),
    calcRow(Col, InnerBoard, Row),
    %if Row = -1 currently it fail
    Row \= -1,
    %set Board[col, row] = 1
    updateBoard(Board, Col, Row, 1, UpdatedBoard).

playComputer(Board, UpdatedBoard) :-
    format("Computer turn!\n-------------\n"),
    % for now - computer choose a random col
    %random(1, 8, Col),
    %calcRow(Col, Board, Row),
    % if row = -1 it backtracks
    %Row \= -1,
    Pos = pos(Board, computer),
    alphabeta(Pos, _, _, GoodPos, _, 4),
    GoodPos = pos(UpdatedBoard, Player).
    %for debugging:
    %UpdatedBoard = Board.

% Check winning constraints

check_row0([], CurWinner, Counter, Winner) :- 
    !,
    (Counter >= 4, !, Winner = CurWinner);
    Winner = none.

check_row0([X|Rest], CurWinner, Counter, Winner) :-
    (
    % if Counter = 4 - set the winner to CurWinner
    Counter >= 4, !, Winner = CurWinner
    );
    (
    % else if current element = 0, then the strike has broken
    % reset the counter and set CurWinner to none
    X = 0, !,
    CurWinner1 = none, Counter1 = 0,
    check_row0(Rest, CurWinner1, Counter1, Winner)
    );
    (
    % else if current element is 1 - then current winner is player
    X = 1, !, CurWinner1 = player,
    % if the old current winner is player - increment counter
    ((CurWinner = player, !, Counter1 is Counter + 1);
    % else - Counter1 = 1
    (Counter1 = 1)),
    check_row0(Rest, CurWinner1, Counter1, Winner)
    );
    (
    % else if current element is 2 - then current winner is computer
    X = 2, !, CurWinner1 = computer,
    % if the old current winner is player - increment counter
    ((CurWinner = computer, !, Counter1 is Counter + 1);
    % else - Counter1 = 1
    (Counter1 = 1)),
    check_row0(Rest, CurWinner1, Counter1, Winner)
    ).


%check_row(-Row, +Winner)
check_row(Row, Winner) :-
    check_row0(Row, none, 0, Winner).
    
check_rows0([], CurWinner, CurWinner) :- !.
check_rows0([Row|RestRows], CurWinner, Winner) :-
    ((check_row(Row, CurWinner1), !, Winner = CurWinner1);
    (CurWinner1 = none, !, check_rows0(RestRows, CurWinner1, Winner))).

% check_rows(-Rows, +Winner)
check_rows(Rows, Winner) :-
    check_rows0(Rows, none, Winner),
    nonvar(Winner).

check_col0([], ColId, CurWinner, Counter, Winner) :- 
    !,
    (Counter >= 4, !, Winner = CurWinner);
    Winner = none.

check_col0([Row|Rest], ColId, CurWinner, Counter, Winner) :-
    % X = Row[ColId]
    nth1(ColId, Row, X),
    ((
    % if Counter = 4 - set the winner to CurWinner
    Counter >= 4, !, Winner = CurWinner
    );
    ((
    % else if current element = 0, then the strike has broken
    % reset the counter and set CurWinner to none
    X = 0, !,
    CurWinner1 = none, Counter1 = 0
    );
    (
    % else if current element is 1 - then current winner is player
    X = 1, !, CurWinner1 = player,
    % if the old current winner is player - increment counter
    ((CurWinner = player, !, Counter1 is Counter + 1);
    % else - Counter1 = 1
    (Counter1 = 1))
    );
    (
    % else if current element is 2 - then current winner is computer
    X = 2, !, CurWinner1 = computer,
    % if the old current winner is player - increment counter
    ((CurWinner = computer, !, Counter1 is Counter + 1);
    % else - Counter1 = 1
    (Counter1 = 1))
    )),
    check_col0(Rest, ColId, CurWinner1, Counter1, Winner)).

check_col(Rows, ColId, Winner) :-
    check_col0(Rows, ColId, none, 0, Winner).

check_cols0(_Rows, ColId, CurWinner, CurWinner) :- ColId > 7, !.
check_cols0(Rows, ColId, CurWinner, Winner) :-
    ((check_col(Rows, ColId, CurWinner1), Winner = CurWinner1, !);
    (CurWinner1 = none, !, ColId1 is ColId + 1, check_cols0(Rows, ColId1, CurWinner1, Winner))).

check_cols(Rows, Winner) :-
    check_cols0(Rows, 1, none, Winner),
    nonvar(Winner).

check_left_diag0([], ColId, CurWinner, Counter, Winner) :- 
    !,
    (Counter >= 4, !, Winner = CurWinner);
    Winner = none.

check_left_diag0(_, ColId, CurWinner, Counter, Winner) :- 
    ColId < 1, !,
    (Counter >= 4, !, Winner = CurWinner);
    Winner = none.

check_left_diag0([Row|Rest], ColId, CurWinner, Counter, Winner) :-
    % X = Row[ColId]
    nth1(ColId, Row, X),
    ColId1 is ColId - 1,
    ((
    % if Counter = 4 - set the winner to CurWinner
    Counter >= 4, !, Winner = CurWinner
    );
    ((
    % else if current element = 0, then the strike has broken
    % reset the counter and set CurWinner to none
    X = 0, !,
    CurWinner1 = none, Counter1 = 0
    );
    (
    % else if current element is 1 - then current winner is player
    X = 1, !, CurWinner1 = player,
    % if the old current winner is player - increment counter
    ((CurWinner = player, !, Counter1 is Counter + 1);
    % else - Counter1 = 1
    (Counter1 = 1))
    );
    (
    % else if current element is 2 - then current winner is computer
    X = 2, !, CurWinner1 = computer,
    % if the old current winner is player - increment counter
    ((CurWinner = computer, !, Counter1 is Counter + 1);
    % else - Counter1 = 1
    (Counter1 = 1))
    )),
    % check in both next element in current diognal and also in another diagonal
    (check_left_diag0(Rest, ColId1, CurWinner1, Counter1, Winner);
    %check in a new diagonal with Rest lines
    check_left_diag0(Rest, ColId, none, 0, Winner);
    %check in a new diagonal with all lines
    check_left_diag0([Row|Rest], ColId1, none, 0, Winner)
    )).

check_right_diag0([], ColId, CurWinner, Counter, Winner) :- 
    !,
    (Counter >= 4, !, Winner = CurWinner);
    Winner = none.

check_right_diag0(_, ColId, CurWinner, Counter, Winner) :- 
    ColId > 7,
    !,
    (Counter >= 4, !, Winner = CurWinner);
    Winner = none.

check_right_diag0([Row|Rest], ColId, CurWinner, Counter, Winner) :-
    % X = Row[ColId]
    nth1(ColId, Row, X),
    ColId1 is ColId + 1,
    ((
    % if Counter = 4 - set the winner to CurWinner
    Counter >= 4, !, Winner = CurWinner
    );
    ((
    % else if current element = 0, then the strike has broken
    % reset the counter and set CurWinner to none
    X = 0, !,
    CurWinner1 = none, Counter1 = 0
    );
    (
    % else if current element is 1 - then current winner is player
    X = 1, !, CurWinner1 = player,
    % if the old current winner is player - increment counter
    ((CurWinner = player, !, Counter1 is Counter + 1);
    % else - Counter1 = 1
    (Counter1 = 1))
    );
    (
    % else if current element is 2 - then current winner is computer
    X = 2, !, CurWinner1 = computer,
    % if the old current winner is player - increment counter
    ((CurWinner = computer, !, Counter1 is Counter + 1);
    % else - Counter1 = 1
    (Counter1 = 1))
    )),
    % check the current diagoanl
    (check_right_diag0(Rest, ColId1, CurWinner1, Counter1, Winner);
    % check a new diagonal
    check_right_diag0(Rest, ColId, none, 0, Winner);
    %check in a new diagonal with all lines
    check_right_diag0([Row|Rest], ColId1, none, 0, Winner)
    )).

check_right_diag(Rows, ColId, Winner) :-
    check_right_diag0(Rows, ColId, none, 0, Winner).

check_left_diag(Rows, ColId, Winner) :-
    check_left_diag0(Rows, ColId, none, 0, Winner).

cutRow([], CurColId, MinColId, MaxColId, []) :- !.

cutRow([X|Rest], CurColId, MinColId, MaxColId, [X|SubCol]) :-
    CurColId >= MinColId,
    CurColId =< MaxColId, !,
    CurColId1 is CurColId + 1,
    cutRow(Rest, CurColId1, MinColId, MaxColId, SubCol).

cutRow([X|Rest], CurColId, MinColId, MaxColId, SubCol) :-
    CurColId1 is CurColId + 1,
    cutRow(Rest, CurColId1, MinColId, MaxColId, SubCol).

% cutBoard0(-InnerBoard, -MinRowId, -MinColId, -MaxColId, +SubBoard)
cutBoard0([], _CurRowId, _MinRowId, _MinColId, _MaxColId, []) :- !.

cutBoard0([Row|Rest], CurRowId, MinRowId, MinColId, MaxColId, [Row1|SubBoard]) :-
    CurRowId >= MinRowId, !,
    CurRowId1 is CurRowId + 1,
    cutRow(Row, 1, MinColId, MaxColId, Row1),
    cutBoard0(Rest, CurRowId1, MinRowId, MinColId, MaxColId, SubBoard).

cutBoard0([Row|Rest], CurRowId, MinRowId, MinColId, MaxColId, SubBoard) :-
    CurRowId < MinRowId,
    CurRowId1 is CurRowId + 1,
    cutBoard0(Rest, CurRowId1, MinRowId, MinColId, MaxColId, SubBoard).

cutBoard(Board, SubBoard) :-
    Board = (MinRowId, MinColId, MaxColId, InnerBoard),
    cutBoard0(InnerBoard, 1, MinRowId, MinColId, MaxColId, SubBoard).

won0(Board, Winner) :-
    Board = (MinRowId, MinColId, MaxColId, InnerBoard),
    cutBoard(Board, InnerBoard1),
    ((MaxColId - MinColId >= 3, check_rows(InnerBoard1, Winner), nonvar(Winner), Winner \= none, !);
    (6 - MinRowId >= 3, check_cols(InnerBoard1, Winner), nonvar(Winner), Winner \= none, !);
    (MaxColId - MinColId >= 3, 6 - MinRowId >= 3, check_right_diag(InnerBoard1, 1, Winner), nonvar(Winner), Winner \= none, !);
    (MaxColId - MinColId >= 3, 6 - MinRowId >= 3, check_left_diag(InnerBoard1, 7, Winner), nonvar(Winner), Winner \= none, !)).

% check if there is a win: 4 in row, in column or in diagonal
won(Board, Winner) :-
    won0(Board, Winner),
    ((Winner = player, !, format("You won!\n--------------\n"));
    (Winner = computer, format("Computer won!\n--------------\n"))).

play0(Board, player, UpdatedBoard) :-
    playUser(Board, UpdatedBoard1), !,
    displayBoard(UpdatedBoard1),
    ((won(UpdatedBoard1, Winner));
    play0(UpdatedBoard1, computer, UpdatedBoard)).

play0(Board, computer, UpdatedBoard) :-
    playComputer(Board, UpdatedBoard1), !,
    displayBoard(UpdatedBoard1),
    (won(UpdatedBoard1, Winner);
    play0(UpdatedBoard1, player, UpdatedBoard)).

play(Board, UpdatedBoard) :-
    displayBoard(Board),
    play0(Board, player, UpdatedBoard).

start :-
    init(Board),
    play(Board, UpdatedBoard).