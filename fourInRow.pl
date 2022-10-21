/******************************************************************************
*__________________________Mamam17 - Final Project____________________________*
*					 20596 - Prolog & Artificial Intelligence				  *				
*------------|----------------------------------------------------------------*														
* Programmer | Uriya Yavniely           									  * 
*------------|----------------------------------------------------------------*
* File Name  | fourinrow.pl													  * 
*------------|----------------------------------------------------------------*
* Description| Four in Row board game application against the computer, 	  *
*			 | implemented with the alpha-beta search algorithm.			  *
*------------|----------------------------------------------------------------*
* Synopsis   | Start the game with the predicate start/0. 			 		  *
* 		     | The game will start & guide you with further instructions.  	  *
* 		     | The goal:                                                      *
*            | Be the first player to connect 4 of the same colored discs     *
*            | in a row (either vertically, horizontally, or diagonally)      *
*            | How to play:                                                   *
*            | On your turn drop one of your colored discs from the top into  *
*            | any of the seven columns. The disk will land on the top of the *
*            | rest of the disks on that column.                              *
*            | The game ends when there is a 4-in-a-row or a tie.             *
*-----------------------------------------------------------------------------*/

/* init(+Board) :- Initializes a board.
        The result board will be a tuple of (MinRowId, MinColId, MaxColId, InnerBoard)
        Where InnerBoard is an empty board of 6 rows and 7 columns.
        The InnerBoard is represented by list of rows, where the first row is the top row
        and the last row is the bottom row. The first items in each row are the left-most column
        and the last items in each row are the right-most column.
        MinRowId is the minimal row index that contains a disk, which is 6 at start
        MinColId is the minimal column index that contains a disk, which is 1 at start
        And MaxColId is the maximal column that contains a disk, which is 1 at start
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

% getInnerBoard(-Board, +InnerBoard) :- extract the inner board from a Board
%   and returns the result in InnerBoard
getInnerBoard((_MinRowId, _MinColId, _MaxColId, InnerBoard), InnerBoard).

% displayLine(-Row) :- print a row of a board to the screen.
%                      Player's cells will be displayed as "o" and in green,
%                      while Computer's cells will be displayed as "#" and in red.
%                      Empty cells will be displayed as spaces.
displayLine([]) :- 
    !, format("\n").

displayLine([X|Rest]) :-
    % decide which character should be displayed, according to the data in the cell
    (
        (X = 0, !, D = " ");
        (X = 1, !, D = "o", FG=green);
        (X = 2, !, D = "#", FG=red)
    ),
    format("["),
    ansi_format([bold, fg(FG)], "~w", D),
    format("]"),
    displayLine(Rest).

% displayLines(-Rows) :- displays each row from Rows with displayLine/1
displayLines([]) :- !.
displayLines([Line|Rest]) :-
    displayLine(Line),
    displayLines(Rest).

% displayBoard(-Board) :- displays the inner board of Board with displayLines/1
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

% calcRow0(-Col, -Board, -CurRow, +Row) :-
%   for a given column Col in Board (inner board),
%   and for a starting row (CurRow) - 
%   calculates the first empty row index in the column and returns the result in Row.
%   If such a row cannot be retrieved - return -1
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

% calcRow(-Col, -Board, +Row) :- calculates the first empty row index in column Col
%   returns the result in Row
calcRow(Col, Board, Row) :-
    calcRow0(Col, Board, 6, Row).

% getCol(+Col, +Action) :- asks the user which column index he want to place the disk in
%   returns the result in Col. If the user wants to exit or restart then Action will be set to
%   "exit" or "restart" respectively (otherwise - Action will not be set) 
getCol(Col, Action) :-
    format("Where do you want to place your token? (1-7, x for exit, r for restart)\n"),
    getAnswer(Ans),
    ((
        Ans = x,
        !, Action = exit
    );
    (
        Ans = r,
        !, Action = restart
    );
    (
        Col1 = Ans,
        (number(Col1), 1 =< Col1, Col1 =< 7, Col = Col1);
        (format("Error: invalid answer\n"), getCol(Col, Action))
    )).

% updateBoard(-Board, -Col, -Row, -Value, +UpdatedBoard) :-
%       set the cell value at column Col and row Row to Value
%       returns the updated board in UpdatedBoard
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

% getCell(-Board, -Row, -Col, +Value) :-
%   get the value of the cell at row Row and column Col
%   returns the result in Value
getCell(Board, Row, Col, Value) :-
    nth1(Row, Board, RowElement),
    nth1(Col, RowElement, Value).

/****  The alpha-beta algorithm ***/
/*
This is the alpha-beta alogirthm from the text book.
I implemented max_to_move/1, moves/2, move/2 and staticval/4.
Position is represented as:
Pos = pos(Board, Player, ColId)
Where Player can be computer or player, 
and ColId is the move that was chosen by the last player, resulted in Board.
Since the player always play first, then he will be Max and the computer will be Min.
*/
% max_to_move(-Pos) :- true is position Pos is followed by the turn of Max player
max_to_move(pos(_, player, _)).
% min_to_move(-Pos) :- true is position Pos is followed by the turn of Min player
min_to_move(pos(_, computer, _)).

% moves(-Pos, +PosList) :- for given Pos - retrieves all the possible positions
moves(Pos, PosList) :-
    (setof(Pos1, move(Pos, Pos1), PosList), !);
    % if there aren't any moves - returns an empty list
    PosList = [].

% move(-Pos, +Pos1) :- for position Pos - return a possible position Pos1 that Pos can be followed by
move(Pos, Pos1) :-
    Pos = pos(Board, Player, _ColId),
    getInnerBoard(Board, InnerBoard),
    % get a Col index and a matching Row
    between(1, 7, ColId1),
    calcRow(ColId1, InnerBoard, RowId1),
    % assert that a matching row for ColId1 can be found,
    % if not - try another Col by using back-tracking
    RowId1 \= -1,
    % choose the value of the new cell, based on who played in Pos
    (
        (Player = player, Player1 = computer, Val = 1);
        (Player = computer, Player1 = player, Val = 2)
    ),
    updateBoard(Board, ColId1, RowId1, Val, UpdatedBoard),
    Pos1 = pos(UpdatedBoard, Player1, ColId1).

% staticval(-Pos, -CurDepth, -MaxDepth, +Val) :-
%       make an assessment of the position Pos by checking if anyone wins
%       if no one win - return 0
staticval(Pos, CurDepth, MaxDepth, Val) :-
    Pos = pos(Board, _Player, _ColId),
    (
        (
            (won0(Board, Winner),
            nonvar(Winner)),
            % Since player is always Max and computer is always Min - 
            % if player win - return a positive value
            % and if computer wins - return a negative value
            % also, give a greater reward on minimal depth (faster win)
            (
                (Winner = player, Val is 1 + MaxDepth - CurDepth);
                (Winner = computer, Val is -1 - (MaxDepth - CurDepth))
            )
        );
        Val is 0
    ).

% alphabeta(-Pos, -Alpha, -Beta, +GoodPos, +Val, -MaxDepth) :-
%   calculates the best move from Pos by using the alphabeta0/7
%   where initial beta = 10 and alpha = -10
%   Returns the best move in GoodPos and its value in Val
alphabeta(Pos, Alpha, Beta, GoodPos, Val, MaxDepth) :- 
    Beta is 10,
    Alpha is -Beta,
    alphabeta0(Pos, Alpha, Beta, GoodPos, Val, 1, MaxDepth).

% alphabeta0(-Pos, -Alpha, -Beta, +GoodPos, +Val, -CurDepth, -MaxDepth) :-
%   calculates the best move from Pos by using the alpha beta algorithm.
%   If current depth (CurDepth) is greater or equal to MaxDepth then returns the staticval
%   
alphabeta0(Pos, Alpha, Beta, GoodPos, Val, CurDepth, MaxDepth) :- 
    % calculate the heuristic value Of Pos 
    staticval(Pos, CurDepth, MaxDepth, Val1),
    (
        % if max depth exceeded or that Pos is a winning state - return Val
        (
        (CurDepth >= MaxDepth;
        Val1 \= 0),
        Val = Val1,
        GoodPos = Pos
        );
        % else - evaluate the children of Pos and choose the best
        (moves(Pos, PosList), !,
            (
                %if PosList is empty - return GoodPos = Pos, Val = Val1
                (
                    not(member(_Elem, PosList)),
                    !, GoodPos = Pos, Val = Val1
                );
                %else - evaluate the children
                (
                    CurDepth1 is CurDepth + 1,
                    boundedbest(PosList, Alpha, Beta, GoodPos, Val, CurDepth1, MaxDepth)
                )
            )
        )
    ).
    
boundedbest([Pos | PosList], Alpha, Beta, GoodPos, GoodVal, CurDepth, MaxDepth) :-
    alphabeta0(Pos, Alpha, Beta, _, Val, CurDepth, MaxDepth), 
    goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, CurDepth, MaxDepth).

goodenough([], _ , _ , Pos, Val, Pos, Val, _CurDepth, _MaxDepth) :- !. % No Other candidate 
    
goodenough( _, Alpha, Beta, Pos, Val, Pos, Val, _CurDepth, _MaxDepth) :-
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

betterof(Pos, Val, _Pos1, Val1, Pos, Val) :- % Pos better than Pos1
    min_to_move(Pos), Val > Val1, !; 
    max_to_move(Pos), Val < Val1, !.

betterof(_, _ , Pos1, Val1, Pos1, Val1). % Otherwise Pos1 better

/******/

% playUser(-Board, +UpdatedBoard, +Action) :- play user turn with Board
%       return the resulted board in UpdatedBoard
%       if the user want to exit or restart - Action will be set to one of them respectively
playUser(Board, UpdatedBoard, Action) :-
    getInnerBoard(Board, InnerBoard),
    format("Player turn!\n-------------\n"),
    % let the user choose a column index
    getCol(Col, Action),
    (
        (
            % if Action is set (either to exit or restart) - exit
            nonvar(Action)
        );
        (
            % get the matching row id for Col
            calcRow(Col, InnerBoard, Row),
            % Row must not be -1
            Row \= -1,
            % set Board[col][row] = 1
            updateBoard(Board, Col, Row, 1, UpdatedBoard)
        )
    ).

% playComputer(-Board, )
playComputer(Board, Level, UpdatedBoard) :-
    format("Computer turn!\nThinking...\n"),
    Pos = pos(Board, computer, _ColId),
    alphabeta(Pos, _, _, GoodPos, _, Level),
    GoodPos = pos(UpdatedBoard, _Player, ColId),
    format("Chose column ~w\n-------------\n", [ColId]).

cutRow([], _CurColId, _MinColId, _MaxColId, []) :- !.

cutRow([X|Rest], CurColId, MinColId, MaxColId, [X|SubCol]) :-
    CurColId >= MinColId,
    CurColId =< MaxColId, !,
    CurColId1 is CurColId + 1,
    cutRow(Rest, CurColId1, MinColId, MaxColId, SubCol).

cutRow([_X|Rest], CurColId, MinColId, MaxColId, SubCol) :-
    CurColId1 is CurColId + 1,
    cutRow(Rest, CurColId1, MinColId, MaxColId, SubCol).

% cutBoard0(-InnerBoard, -MinRowId, -MinColId, -MaxColId, +SubBoard)
cutBoard0([], _CurRowId, _MinRowId, _MinColId, _MaxColId, []) :- !.

cutBoard0([Row|Rest], CurRowId, MinRowId, MinColId, MaxColId, [Row1|SubBoard]) :-
    CurRowId >= MinRowId, !,
    CurRowId1 is CurRowId + 1,
    cutRow(Row, 1, MinColId, MaxColId, Row1),
    cutBoard0(Rest, CurRowId1, MinRowId, MinColId, MaxColId, SubBoard).

cutBoard0([_Row|Rest], CurRowId, MinRowId, MinColId, MaxColId, SubBoard) :-
    CurRowId < MinRowId,
    CurRowId1 is CurRowId + 1,
    cutBoard0(Rest, CurRowId1, MinRowId, MinColId, MaxColId, SubBoard).

cutBoard(Board, SubBoard) :-
    Board = (MinRowId, MinColId, MaxColId, InnerBoard),
    cutBoard0(InnerBoard, 1, MinRowId, MinColId, MaxColId, SubBoard).

won0(Board, Winner) :-
    Board = (MinRowId, MinColId, MaxColId, _InnerBoard),
    cutBoard(Board, InnerBoard1),
    MaxCutColId is MaxColId - MinColId + 1,
    (
        (
            % check rows
            MaxCutColId >= 4,
            nth1(_I, InnerBoard1, Row),
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
    (won0(Board, Winner),
    nonvar(Winner),
    Winner \= none,
    !,
    ((Winner = player, !, ansi_format([bold, fg(green)], "You won!\n", []));
    (Winner = computer, ansi_format([bold, fg(red)], "Computer won!\n", []))));
    (is_tie(Board)).

play0(Board, player, Level, UpdatedBoard, Action) :-
    playUser(Board, UpdatedBoard1, Action), !,
    (
        (
            nonvar(Action)
        );
        (
        displayBoard(UpdatedBoard1),
        ((won(UpdatedBoard1, _Winner));
        play0(UpdatedBoard1, computer, Level, UpdatedBoard, Action))
        )
    ).

play0(Board, computer, Level, UpdatedBoard, Action) :-
    playComputer(Board, Level, UpdatedBoard1), !,
    displayBoard(UpdatedBoard1),
    (won(UpdatedBoard1, _Winner);
    play0(UpdatedBoard1, player, Level, UpdatedBoard, Action)).

play(Board, Level, UpdatedBoard, Action) :-
    displayBoard(Board),
    play0(Board, player, Level, UpdatedBoard, Action).

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
    % initializes an empty 6x7 board
    init(Board),
    printInstructions,
    % let the user choose the difficult level
    chooseLevel(Level),
    % start playing
    % this predicate will call recursively to all play turns
    % it will exit only if someone win or if the user asked to exit or restart the game
    play(Board, Level, _UpdatedBoard, Action),
    (
        % if the Action is exit - exit the game
        (Action = exit);
        % otherwise, if the Action is restart - then restart by calling recursively to start
        (Action = restart, start)
    ).