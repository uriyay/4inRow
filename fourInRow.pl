/*
initial cell: 0
player cell: 1
computer cell: 2
*/

init(Board) :-
    % 7 columns and 6 rows
    Board = [
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0]
    ].

displayBoard(Board) :-
    Board = [R1, R2, R3, R4, R5, R6],
    format("~w\n~w\n~w\n~w\n~w\n~w\n", [R1, R2, R3, R4, R5, R6]).

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
    %get RowElement at Row index
    nth1(Row, Board, RowElement, RestRows),
    %get the row without the cell
    nth1(Col, RowElement, _Cell, RowElementRest),
    %build a new row from RowElementRest where RowElement1[Col] = Value
    nth1(Col, RowElement1, Value, RowElementRest),
    %build a new UpdatedBoard which at Row there is RowElement1
    nth1(Row, UpdatedBoard, RowElement1, RestRows).

getCell(Board, Row, Col, Value) :-
    nth1(Row, Board, RowElement),
    nth1(Col, RowElement, Value).

playUser(Board, UpdatedBoard) :-
    format("Player turn!\n-------------\n"),
    getCol(Col),
    calcRow(Col, Board, Row),
    %if Row = -1 currently it fail
    Row \= -1,
    %set Board[col, row] = 1
    updateBoard(Board, Col, Row, 1, UpdatedBoard).

playComputer(Board, UpdatedBoard) :-
    format("Computer turn!\n-------------\n"),
    % for now - computer choose a random col
    random(1, 8, Col),
    calcRow(Col, Board, Row),
    % if row = -1 it backtracks
    Row \= -1,
    updateBoard(Board, Col, Row, 2, UpdatedBoard).
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

% check if there is a win: 4 in row, in column or in diagonal
won(Board, Winner) :-
    ((check_rows(Board, Winner), nonvar(Winner), Winner \= none, !);
    (check_cols(Board, Winner), nonvar(Winner), Winner \= none, !);
    (check_right_diag(Board, 1, Winner), nonvar(Winner), Winner \= none, !);
    (check_left_diag(Board, 7, Winner), nonvar(Winner), Winner \= none, !)),
    ((Winner = player, !, format("You won!\n--------------\n"));
    (Winner = computer, format("Computer won!\n--------------\n"))).

play0(Board, player, UpdatedBoard) :-
    displayBoard(Board),
    playUser(Board, UpdatedBoard1), !,
    ((won(UpdatedBoard1, Winner), displayBoard(UpdatedBoard1));
    play0(UpdatedBoard1, computer, UpdatedBoard)).

play0(Board, computer, UpdatedBoard) :-
    displayBoard(Board),
    playComputer(Board, UpdatedBoard1), !,
    (won(UpdatedBoard1, Winner);
    play0(UpdatedBoard1, player, UpdatedBoard)).

play(Board, UpdatedBoard) :-
    play0(Board, player, UpdatedBoard).

start :-
    init(Board),
    play(Board, UpdatedBoard).