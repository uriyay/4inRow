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
    format("~w\n", Board).

/* getAnswer(+Answer) - reads one-char answer and returns it as an atomic (for example: "1" -> 1) */
getAnswer(Answer) :-
    format("> "),
    get(AnswerAscii),
    name(Answer, [AnswerAscii]).

calcRow0(Col, Board, CurRow, Row) :-
    (CurRow < 0, Row = -1);
    (nth1(CurRow, Board, RowElement),
    nth1(Col, RowElement, Cell),
    %if Cell = 0 then return Row = CurRow
    (Cell = 0, Row = CurRow);
    %else if calcRow0 returns a valid row - return it
    (CurRow1 is CurRow - 1, calcRow0(Col, Board, CurRow1, Row));
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
    nth1(Col, RowElement, Cell, RowElementRest),
    %build a new row from RowElementRest where RowElement1[Col] = Value
    nth1(Col, RowElement1, Value, RowElementRest),
    %build a new UpdatedBoard which at Row there is RowElement1
    nth1(Row, UpdatedBoard, RowElement1, RestRows).

playUser(Board, UpdatedBoard) :-
    getCol(Col),
    calcRow(Col, Board, Row),
    %set Board[col, row] = 1
    updateBoard(Board, Col, Row, 1, UpdatedBoard).
    
% check if there is a win: 4 in row, in column or in diagonal
won(Board, player) :-
    fail. %

won(Board, computer) :-
    fail. %

play0(Board, player, UpdatedBoard) :-
    displayBoard(Board),
    playUser(Board, UpdatedBoard1),
    play0(UpdatedBoard1, computer, UpdatedBoard).

play0(Board, computer, UpdatedBoard) :-
    displayBoard(Board),
    playComputer(Board, UpdatedBoard1),
    play0(UpdatedBoard1, player, UpdatedBoard).

play(Board, UpdatedBoard) :-
    play0(Board, player, UpdatedBoard).

start :-
    init(Board),
    play(Board, UpdatedBoard).