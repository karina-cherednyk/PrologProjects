:- use_module(library(clpfd)). % import transpose

zero(0).
len([], 0).
len([_|Xs], L) :- len(Xs,L1), L is L1 + 1.

fill(Size, Elem, List ):- fill(0, Size, Elem,[], List).

fill(Size, Size, _, List, List) :- !.

fill(I, Size, Elem, ListPrev, List):-
    I1 is I + 1,
    append([Elem], ListPrev, List1),
    fill(I1, Size, Elem, List1, List).

fill_to(Size, ListPrev, List) :-
    len(ListPrev, Size1),
    Delta is Size - Size1,
    fill(Delta, 0, List1),
    append(ListPrev, List1, List).


replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% ----------------------------------

combine(Size, BoardPrev, Board, Score) :-
    fill(Size, Size,SizeArg), maplist(combine_row, SizeArg, BoardPrev, Board, Scores),
    sum_list(Scores, Score).

stack(Size, BoardPrev, Board) :-
     fill(Size, Size,SizeArg), maplist(stack_row, SizeArg, BoardPrev, Board).

stack_row(Size, RowPrev, Row) :-
    exclude(zero, RowPrev, Row1),
    fill_to(Size, Row1, Row).

combine_row(_, [], [], 0).

combine_row(Size, [Val1, Val1|RowLeft], [0|Row1], Score) :- !,
    Val2 is 2*Val1,
    combine_row(Size, [Val2|RowLeft], Row1, Score1),
    Score is Val2 + Score1.

combine_row(Size, [Val1|RowLeft], [Val1|Row1], Score1 ) :- combine_row(Size, RowLeft,Row1, Score1).

reverse_rows(BoardPrev, Board) :-
    maplist(reverse, BoardPrev, Board).

b([[0,0,0,0],[0,2,2,4],[2,4,2,2],[4,0,4,8]]).

% ------------- moves ----------------

left(Size, BoardPrev, Board, Score) :-
    combine(Size, BoardPrev, Board1, Score1),
    stack(Size, Board1, Board2),
    ( Board2=BoardPrev ->
             Board=BoardPrev, Score=Score1 ;
             left(Size, Board2, Board, Score2), Score is Score1 + Score2 ).


right(Size, BoardPrev, Board, Score) :-
    reverse_rows(BoardPrev, Board1),
    left(Size, Board1, Board2, Score),
    reverse_rows(Board2, Board).

up(Size, BoardPrev, Board, Score) :-
    transpose(BoardPrev, Board1),
    left(Size, Board1, Board2, Score),
    transpose(Board2, Board).

down(Size, BoardPrev, Board, Score) :-
    transpose(BoardPrev, Board1), reverse_rows(Board1, Board2),
    left(Size, Board2, Board3, Score),
    reverse_rows(Board3, Board4), transpose(Board4, Board).


has_moves(Board) :- not(member(0, Board)).

max_value(Board, MaxValue) :-
    flatten(Board, Values), max_member(MaxValue, Values).

get4_2(Res) :- ( random(1, 11, C), C < 10 -> Res=2 ; Res=4).

new_cell(Size, BoardPrev, Board) :-
    flatten(BoardPrev, FBoard),
    findall(I, nth0(I, FBoard, 0), AvailableCells),
    len(AvailableCells, AvailableSize),
    random(0, AvailableSize, NewIndex),
    get4_2(NewValue),
    replace(FBoard, NewIndex, NewValue, NewFBoard),
    matrix(Size, NewFBoard, Board).


matrix(Size, FBoard, Board) :- matrix(0, Size, FBoard, [], Board).
matrix(Cols, Cols, [], NewRow, [NewRow]) :- !.

matrix(Cols, Cols, PrevList, NewRow, [NewRow|List1]) :-
    matrix(0, Cols, PrevList, [], List1), !.

matrix(I, Cols, [X|PrevList], NewRow, List) :-
    append(NewRow, [X], NewRow1),
    I1 is I + 1,
    matrix(I1, Cols, PrevList, NewRow1, List).

move(left).
move(up).
move(right).
move(down).




can_move(Size, Board) :- move(Move), can_move(Move, Size, Board).

can_move(Move, Size, Board) :- call(Move, Size, Board, Board1, _), Board1 \= Board.

% -------------- user game --------------
show_board([]).
show_board([Row|LeftBoard]) :-
    writeln(Row), show_board(LeftBoard).

get_move(Val) :-
    writeln('0 - Left; 1 - Right; 2 - Up, 3 - Down'), read(Val1),
    (integer(Val1), Val1>=0, Val1 <4 -> Val=Val1 ; get_move(Val) ).

user_move(Size, BoardPrev, Board, Score) :-
    get_move(MoveI),
    Moves=[left, right, up, down],
    nth0(MoveI,Moves, Move),
    (can_move(Move, Size, BoardPrev) ->
           call(Move, Size, BoardPrev, Board, Score);
           writeln('Can`t move to that direction'),
           user_move(Size, BoardPrev, Board, Score) ).

game(Size, Board) :-
   ( not(has_moves(Board)) ; not(can_move(Size, Board)) ),
     max_value(Board, MaxValue),
     writef('Game ended. max value is: %t', [MaxValue] ), !.

game(Size, Board, UserScorePrev) :-
    show_board(Board),
    new_cell(Size, Board, Board1),
    user_move(Size, Board1, Board2, UMoveScore),
    UserScore is UserScorePrev + UMoveScore,
    writef('User scored %t\n', [UserScore]),
    game(Size, Board2, UserScore).

start() :- b(B), game(4, B, 0).
% ------------------ ai -----------------
