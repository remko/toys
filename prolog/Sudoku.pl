%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sudoku.pl
%
% This is a little experiment to create a Sudoku puzzle in Prolog.
% I'm sure that by now, millions of other (and better) programs like this exist.
% Works under SWI-Prolog.
%
% Remko Tronçon
% http://el-tramo.be
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(lists)).
:- use_module(library('clp/bounds')).

board([
	_, 9, 3,   7, _, 6,   _, _, _,
	_, _, _,   _, _, _,   _, 9, _,
	_, _, _,   _, 3, 1,   4, _, 8,

	_, 5, 4,   6, _, _,   _, _, _,
	_, 6, _,   _, _, 5,   _, _, 2,
	2, _, 1,   _, _, _,   _, _, 9,

	_, _, _,   _, 8, _,   _, 6, _,
	4, _, 8,   1, _, _,   5, _, _,
	_, _, 5,   _, _, _,   _, _, 3
]).  

sudoku :-
	board(Board),
	sudoku_clp(Board),
	write_board(Board).

sudoku_count :-
	board(Board),
	findall(_,sudoku_clp(Board),L),
	length(L,Count),
	write(Count), write(' solution(s) possible'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Brute force version (waaay too inefficient)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sudoku_bf(Board) :-
	assign_values(Board),
	board_rows(Board,Rows),
	test_all(Rows),
	board_columns(Board,Columns),
	test_all(Columns),
	board_blocks(Board,Blocks),
	test_all(Blocks).

assign_values([]).
assign_values([X|Xs]) :-
	member(X,[1,2,3,4,5,6,7,8,9]),
	assign_values(Xs).
	
test_all([]).
test_all([X|Xs]) :-
	test(X),
	test_all(Xs).

test([]).
test([X|Xs]) :-
	\+ member(X,Xs),
	test(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraints version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sudoku_clp(Board) :-
	board_rows(Board,Rows),
	board_columns(Board,Columns),
	board_blocks(Board,Blocks),
	constrain_all(Rows),
	constrain_all(Columns),
	constrain_all(Blocks),
	label(Board).

constrain_all([]).
constrain_all([X|Xs]) :-
	constrain(X),
	constrain_all(Xs).

constrain(Vars) :-
	Vars in 1..9,
	all_different(Vars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Board manipulation / inspection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_board(Board) :-
	length(Board,81).
	
board_rows(Board,Rows) :-
	board_rows(8,Board,Rows).

board_rows(N,Board,Rows) :-
	( N >= 0 ->
		NMax is N + 1,
		range(N,NMax,0,9,Board,Row),
		NN is N - 1,
		Rows = [Row|NRows],
		board_rows(NN,Board,NRows)
	;
		Rows = []
	).
	
board_columns(Board,Columns) :-
	board_columns(8,Board,Columns).

board_columns(N,Board,Columns) :-
	( N >= 0 ->
		NMax is N + 1,
		range(0,9,N,NMax,Board,Column),
		NN is N - 1,
		Columns = [Column|NColumns],
		board_columns(NN,Board,NColumns)
	;
		Columns = []
	).
	
board_blocks(Board,Blocks) :-
	board_blocks(8,Board,Blocks).
	
board_blocks(N,Board,Blocks) :-
	( N >= 0 ->
		RowStart is 3*(N // 3),
		RowEnd is RowStart + 3,
		ColStart is 3*(N mod 3),
		ColEnd is ColStart + 3,
		range(RowStart,RowEnd,ColStart,ColEnd,Board,Block),
		NN is N - 1,
		Blocks = [Block|NBlocks],
		board_blocks(NN,Board,NBlocks)
	;
		Blocks = []
	).

range(RowBegin,RowEnd,ColBegin,ColEnd,Board,Elements) :-
	range(RowBegin,ColBegin,RowEnd,ColBegin,ColEnd,Board,Elements).

range(CurRow,CurCol,RowEnd,ColBegin,ColEnd,Board,Elements) :-
	( CurRow < RowEnd ->
		( CurCol < ColEnd ->
			get(CurRow,CurCol,Board,X),
			Elements = [X|Xs],
			NCurCol is CurCol + 1,
			range(CurRow,NCurCol,RowEnd,ColBegin,ColEnd,Board,Xs)
		;
			NCurCol = ColBegin,
			NCurRow is CurRow + 1,
			range(NCurRow,NCurCol,RowEnd,ColBegin,ColEnd,Board,Elements)
		)
	;
		Elements = []
	).

get(Row,Column,Board,Element) :-
	Index is Row*9 + Column,
	nth0(Index,Board,Element).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Board pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_board(Board) :-
	write_line,
	write_board(Board,0).

write_board([],_).
write_board([X|Xs],N) :-
	write(X),
	NN is N + 1,
	( NN mod 3 =:= 0 ->
		( NN mod 9 =:= 0 ->
			nl,
			( NN mod 27 =:= 0 ->
				write_line
			;
				true
			)
		;
			write(' | ')
		)
	; 
		write(' ')
	),
	write_board(Xs,NN).

write_line :-
	write_line(20).

write_line(N) :-
	( N >= 0 ->
		write('-'),
		NN is N - 1,
		write_line(NN)
	;
		nl
	).
