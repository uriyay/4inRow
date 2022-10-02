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
    (X = 1, !, D = "o", FG=green);
    (X = 2, !, D = "#", FG=red)),
    format("["),
    ansi_format([bold, fg(FG)], "~w", D),
    format("]"),
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
Pos = pos(Board, Player, ColId)
where Player can be computer or player
and ColId is the move that was chosen by the last player, resulted in Board
Since the player always play first, then he will be Max and the computer will be Min
*/
max_to_move(pos(_, player, _)).
min_to_move(pos(_, computer, _)).

% moves(-Pos, +PosList) :- for given Pos - retrieve all the possible positions
moves(Pos, PosList) :-
    (setof(Pos1, move(Pos, Pos1), PosList), !);
    PosList = [].

move(Pos, Pos1) :-
    Pos = pos(Board, Player, _ColId),
    getInnerBoard(Board, InnerBoard),
    % when choosing a column it doesn't matter who the player is
    between(1, 7, ColId1),
    calcRow(ColId1, InnerBoard, RowId1),
    RowId1 \= -1,
    ((Player = player, Player1 = computer, Val = 1);
    (Player = computer, Player1 = player, Val = 2)),
    updateBoard(Board, ColId1, RowId1, Val, UpdatedBoard),
    Pos1 = pos(UpdatedBoard, Player1, ColId1).

staticval(Pos, CurDepth, MaxDepth, Val) :-
    Pos = pos(Board, _Player, _ColId),
    (((won1(Board, Winner),
    nonvar(Winner)),
    %since player is always max and computer is always min - 
    %if player win - return a positive value
    %and if computer wins - return a negative value
    %also, give a greater reward on minimal depth (faster win)
    ((Winner = player, Val is 1 + MaxDepth - CurDepth);
    (Winner = computer, Val is -1 - (MaxDepth - CurDepth))));
    Val is 0).

alphabeta(Pos, Alpha, Beta, GoodPos, Val, MaxDepth) :- 
    Beta is 10,
    Alpha is -Beta,
    alphabeta0(Pos, Alpha, Beta, GoodPos, Val, 1, MaxDepth).

alphabeta0(Pos, Alpha, Beta, GoodPos, Val, CurDepth, MaxDepth) :- 
    staticval(Pos, CurDepth, MaxDepth, Val1), % Static value Of Pos 
    (
    % if max depth exceeded or that Pos is a winning state - return Val
    (
    (CurDepth >= MaxDepth;
    Val1 \= 0),
    Val = Val1,
    GoodPos = Pos
    );
    % else - evaluate the children of Pos
    (moves(Pos, PosList), !,
    (
    %if PosList is empty - return GoodPos = Pos, Val = Val1
    (not(member(_Elem, PosList)), !, GoodPos = Pos, Val = Val1);
    %else - evaluate the children
    (CurDepth1 is CurDepth + 1,
    boundedbest(PosList, Alpha, Beta, GoodPos, Val, CurDepth1, MaxDepth)))
    )).
    
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

playComputer(Board, Level, UpdatedBoard) :-
    format("Computer turn!\nThinking...\n"),
    Pos = pos(Board, computer, _ColId),
    alphabeta(Pos, _, _, GoodPos, _, Level),
    GoodPos = pos(UpdatedBoard, Player, ColId),
    format("Chose column ~w\n-------------\n", [ColId]).

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

check_cols0(_Rows, ColId, MaxColId, CurWinner, CurWinner) :- ColId > MaxColId, !.
check_cols0(Rows, ColId, MaxColId, CurWinner, Winner) :-
    ((check_col(Rows, ColId, CurWinner1), Winner = CurWinner1, !);
    (CurWinner1 = none,
    !,
    ColId1 is ColId + 1,
    check_cols0(Rows, ColId1, MaxColId, CurWinner1, Winner))).

check_cols(Rows, MaxColId, Winner) :-
    check_cols0(Rows, 1, MaxColId, none, Winner),
    nonvar(Winner).

check_left_diag0([], ColId, CurWinner, Counter, Winner) :- 
    !,
    (Counter >= 4, !, Winner = CurWinner);
    Winner = none.

check_left_diag0(_, ColId, CurWinner, Counter, Winner) :- 
    ColId < 1, !,
    ((Counter >= 4, !, Winner = CurWinner);
    Winner = none).

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
    ((check_left_diag0(Rest, ColId1, CurWinner1, Counter1, Winner), Winner \= none);
    %check in a new diagonal with Rest lines
    (check_left_diag0(Rest, ColId, none, 0, Winner), Winner \= none);
    %check in a new diagonal with all lines
    (check_left_diag0([Row|Rest], ColId1, none, 0, Winner), Winner \= none)
    )).

check_right_diag0([], ColId, MaxColId, CurWinner, Counter, Winner) :- 
    !,
    (Counter >= 4, !, Winner = CurWinner);
    Winner = none.

check_right_diag0(_, ColId, MaxColId, CurWinner, Counter, Winner) :- 
    ColId > MaxColId,
    !,
    ((Counter >= 4, !, Winner = CurWinner);
    Winner = none).

check_right_diag0([Row|Rest], ColId, MaxColId, CurWinner, Counter, Winner) :-
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
    ((check_right_diag0(Rest, ColId1, MaxColId, CurWinner1, Counter1, Winner), Winner \= none);
    % check a new diagonal
    (check_right_diag0(Rest, ColId, MaxColId, none, 0, Winner), Winner \= none);
    %check in a new diagonal with all lines
    (check_right_diag0([Row|Rest], ColId1, MaxColId, none, 0, Winner), Winner \= none)
    )).

check_right_diag(Rows, ColId, MaxColId, Winner) :-
    check_right_diag0(Rows, ColId, MaxColId, none, 0, Winner).

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
    MaxCutColId is MaxColId - MinColId + 1,
    (
        (
            MaxCutColId >= 4,
            check_rows(InnerBoard1, Winner),
            nonvar(Winner), Winner \= none, !
        );
        (
            6 - MinRowId + 1 >= 4,
            check_cols(InnerBoard1, MaxCutColId, Winner),
            nonvar(Winner), Winner \= none, !
        );
        (
            MaxCutColId >= 4, 6 - MinRowId >= 3,
            check_right_diag(InnerBoard1, 1, MaxCutColId, Winner),
            nonvar(Winner), Winner \= none, !
        );
        (
            MaxCutColId >= 4, 6 - MinRowId >= 3,
            check_left_diag(InnerBoard1, MaxCutColId, Winner),
            nonvar(Winner), Winner \= none, !
        )
    ).

won1(Board, Winner) :-
    Board = (MinRowId, MinColId, MaxColId, InnerBoard),
    cutBoard(Board, InnerBoard1),
    MaxCutColId is MaxColId - MinColId + 1,
    (
        (
            % check rows
            MaxCutColId >= 4,
            nth1(I, InnerBoard1, Row),
            Limit is MaxCutColId - 4 + 1,
            between(1, Limit, J),
            nth1(J, Row, C),
            J1 is J + 1,
            nth1(J1, Row, C),
            J2 is J1 + 1,
            nth1(J2, Row, C),
            J3 is J2 + 1,
            nth1(J3, Row, C),
            C \= 0,
            !
        );
        (
            % check cols
            6 - MinRowId + 1 >= 4,
            nth1(I, InnerBoard1, Row1),
            nth1(J, Row1, C),
            I1 is I + 1,
            nth1(I1, InnerBoard1, Row2),
            nth1(J, Row2, C),
            I2 is I1 + 1,
            nth1(I2, InnerBoard1, Row3),
            nth1(J, Row3, C),
            I3 is I2 + 1,
            nth1(I3, InnerBoard1, Row4),
            nth1(J, Row4, C),
            C \= 0,
            !
        );
        (
            % check right diag
            MaxCutColId >= 4, 6 - MinRowId >= 3,
            nth1(I, InnerBoard1, Row1),
            nth1(J, Row1, C),
            I1 is I + 1,
            J1 is J + 1,
            nth1(I1, InnerBoard1, Row2),
            nth1(J1, Row2, C),
            I2 is I1 + 1,
            J2 is J1 + 1,
            nth1(I2, InnerBoard1, Row3),
            nth1(J2, Row3, C),
            I3 is I2 + 1,
            J3 is J2 + 1,
            nth1(I3, InnerBoard1, Row4),
            nth1(J3, Row4, C),
            C \= 0,
            !
        );
        (
            % check left diag
            MaxCutColId >= 4, 6 - MinRowId >= 3,
            nth1(I, InnerBoard1, Row1),
            nth1(J, Row1, C),
            I1 is I + 1,
            J1 is J - 1,
            nth1(I1, InnerBoard1, Row2),
            nth1(J1, Row2, C),
            I2 is I1 + 1,
            J2 is J1 - 1,
            nth1(I2, InnerBoard1, Row3),
            nth1(J2, Row3, C),
            I3 is I2 + 1,
            J3 is J2 - 1,
            nth1(I3, InnerBoard1, Row4),
            nth1(J3, Row4, C),
            C \= 0,
            !
        )
    ),
    (
        (C = 1, !, Winner = player);
        (C = 2, !, Winner = computer)
    ).

is_tie(Board) :-
    Board = (MinRowId, MinColId, MaxColId, InnerBoard),
    MinRowId = 1,
    MinColId = 1,
    MaxColId = 7,
    InnerBoard = [FirstRow|_Tail],
    not(member(0, FirstRow)),
    ansi_format([bold, fg(blue)], "Tie!\n", []).

% check if there is a win: 4 in row, in column or in diagonal
won(Board, Winner) :-
    (won1(Board, Winner),
    nonvar(Winner),
    Winner \= none,
    !,
    ((Winner = player, !, ansi_format([bold, fg(green)], "You won!\n", []));
    (Winner = computer, ansi_format([bold, fg(red)], "Computer won!\n", []))));
    (is_tie(Board)).

play0(Board, player, Level, UpdatedBoard) :-
    playUser(Board, UpdatedBoard1), !,
    displayBoard(UpdatedBoard1),
    ((won(UpdatedBoard1, Winner));
    play0(UpdatedBoard1, computer, Level, UpdatedBoard)).

play0(Board, computer, Level, UpdatedBoard) :-
    playComputer(Board, Level, UpdatedBoard1), !,
    displayBoard(UpdatedBoard1),
    (won(UpdatedBoard1, Winner);
    play0(UpdatedBoard1, player, Level, UpdatedBoard)).

play(Board, Level, UpdatedBoard) :-
    displayBoard(Board),
    play0(Board, player, Level, UpdatedBoard).

chooseLevel(Level) :-
    format("Choose difficult level:\n1) Easy\n2) Medium\n3) Hard\n"),
    getAnswer(Level1),
    (
        (between(1, 3, Level1),
        % Easy = 2, Medium = 4, Hard = 6
        Level is Level1 * 2);
        (format("Invalid level\n"),
        chooseLevel(Level))
    ).

printInstructions :-
    ansi_format([bold], "Welcome to Four in a Row game!\n\n", []),
    ansi_format([bold], "The goal:\n", []),
    format("Be the first player to connect 4 of the same colored discs in a row (either vertically, horizontally, or diagonally)\n\n"),
    ansi_format([bold], "How to play:\n", []),
    format("* On your turn drop one of your colored discs from the top into any of the seven columns.\nThe disk will land on the top of the rest of the disks on that column\n"),
    format("* The game ends when there is a 4-in-a-row or a tie\n\n").

start :-
    init(Board),
    printInstructions,
    chooseLevel(Level),
    play(Board, Level, UpdatedBoard).