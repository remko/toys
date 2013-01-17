#
# This program was used to generate the maze on the cover of the book
#			'The First 10 Prolog Programming Contests' 
#			( available on http://www.cs.kuleuven.be/~dtai/ppcbook/ )
# It generates a MetaPost file with a perfect maze in it.
#
#		Remko Tronçon
#		http://el-tramo.be
#

maze1 :-
	maze(42,59,[(5,37)-(38,53),(18,4)-(38,16)]).

maze :-
	maze(63,89,[(7,59)-(57,80),(28,6)-(57,25)]).

maze3 :-
	maze(84,118,[(9,74)-(76,106),(36,8)-(76,32)]).

maze(Width,Height,Islands) :-
	all_walls(Width,Height,Walls),
	init_corners(Islands,Walls,[],Walls1,Visited),
	StartPoint = (1,1),
	ToDo = Visited,
	walk(StartPoint,Width,Height,Islands,ToDo,Visited,Walls1,_,Walls2), 
	write_maze(Width,Height,Islands,Walls2).

all_walls(Width,Height,Walls) :-
	Width1 is Width - 1,
	Height1 is Height - 1,
	findall((X,Y)-(NX,Y),
		(for(Y,1,Height),for(X,1,Width1),NX is X + 1), HorizWalls),
	findall((X,Y)-(X,NY),
		(for(Y,1,Height1),for(X,1,Width),NY is Y + 1), VertWalls),
	append(HorizWalls,VertWalls,Walls).

% Haalt een paar muren weg rond de rounded corners
init_corners([],Walls,Visited,Walls,Visited).
init_corners([(X1,Y1)-(X2,Y2)|Cs],Walls,Visited,NWalls,NVisited) :-
	NX1 is X1 - 1,
	NY1 is Y1 - 1,
	TX1 is NX1 + 1, TX2 is X2 + 1, 
	TY1 is NY1 + 1, TY2 is Y2 + 1, 
	select((NX1,NY1)-(NX1,TY1),Walls,Walls1),
	select((NX1,NY1)-(TX1,NY1),Walls1,Walls2),
	select((NX1,Y2)-(NX1,TY2),Walls2,Walls3),
	select((NX1,TY2)-(TX1,TY2),Walls3,Walls4),
	select((X2,TY2)-(TX2,TY2),Walls4,Walls5),
	select((TX2,Y2)-(TX2,TY2),Walls5,Walls6),
	select((TX2,NY1)-(TX2,TY1),Walls6,Walls7),
	select((X2,NY1)-(TX2,NY1),Walls7,Walls8),
	Visited1 = [(NX1,NY1),(NX1,TY2),(TX2,NY1),(TX2,TY2)|Visited],
	init_corners(Cs,Walls8,Visited1,NWalls,NVisited).

walk(Point,Width,Height,Islands,ToDo,Visited,Walls,NVisited,NWalls) :-
	findall(P,next_point(Point,Width,Height,Islands,Visited,P),NextPoints),
	( random_element(NextPoints,NextPoint) ->
		once((select(Point-NextPoint,Walls,Walls1)
			 ; select(NextPoint-Point,Walls,Walls1))),
		walk(NextPoint,Width,Height,Islands,[Point|ToDo],[Point|Visited],Walls1,NVisited,NWalls)
	;
		( ToDo = [T|Ts] ->
			walk(T,Width,Height,Islands,Ts,[Point|Visited],Walls,NVisited,NWalls)
		;
			NVisited = Visited,
			NWalls = Walls
		)
	).

next_point(Point,Width,Height,Islands,Visited,NextPoint) :-
	neighbour(Point,NextPoint),
	NextPoint = (X,Y),
	X >= 1, X =< Width, Y >= 1, Y =< Height,
	\+(on_island(NextPoint,Islands)),
	\+(member(NextPoint,Visited)).

on_island((X,Y),Islands) :-
	member((X1,Y1)-(X2,Y2),Islands), 
	X >= X1, X =< X2, Y >= Y1, Y =< Y2,
	!.
	

random_element(List,Element) :-
	length(List,Length),
	Length > 0,
	Random is random(Length),
	%random(0,Length,Random),
	nth0(Random,List,Element).

neighbour((I,J),(NextI,NextJ)) :- NextI = I, NextJ is J + 1.
neighbour((I,J),(NextI,NextJ)) :- NextI = I, NextJ is J - 1.
neighbour((I,J),(NextI,NextJ)) :- NextJ = J, NextI is I + 1.
neighbour((I,J),(NextI,NextJ)) :- NextJ = J, NextI is I - 1.

for(I,I,J) :- 
	I =< J.
for(K,I,J) :- 
	I < J,
	I1 is I + 1,
	for(K,I1,J).

% Voor hipP
%:- use_module(library(lists)).
%append(A,B,C) :- list_append(A,B,C).
%select(A,B,C) :- once_delete(A,B,C).
%member(A,B) :- logical_member(A,B).
%nth0(A,B,C) :- N is A + 1, list_nth(N,B,C).

%
% MetaPost
%
write_maze(Width,Height,Islands,Walls) :-
	tell('maze.mp'),
	write('beginfig(1);'), nl,
	Unit is 255 / Height,
	write('u = '), write(Unit), write('mm;'), nl,
	write('pickup pencircle scaled (1.2pt);'), nl,
	fill(rectangle(0,0,Width,Height), '0.1[white, black]'),
	draw(rectangle(0,0,Width,Height)),
	nl,
	%Islands = [FirstIsland|RestIslands],
	%write_first_island(FirstIsland),
	%nl,
	%write('pickup pencircle scaled (1.2pt);'), nl,
	%write_islands(RestIslands),
	write_islands(Islands),
	nl,
	write_walls(Walls,Islands),
	write('endfig;'), nl,
	write('end;'), nl,
	told.

write_first_island((OX1,OY1)-(X2,Y2)) :-
	X1 is OX1 - 1,
	Y1 is OY1 - 1,
	Rad = 2,
	write('pickup pencircle scaled (2.5pt);'), nl,
	unfill(rounded_rectangle(X1,Y1,X2,Y2,Rad)),
	draw(rounded_rectangle(X1,Y1,X2,Y2,Rad)),
	write('pickup pencircle scaled (1.2pt);'), nl,
	unfill(rounded_rectangle(X1,Y1,X2,Y2,Rad)),
	draw(rounded_rectangle(X1+0.5,Y1+0.5,X2-0.5,Y2-0.5,Rad)).
	
fill(Command,Color) :-
	write('fill '), 
	call(Command),
	write(' withcolor '), write(Color),
	write(';'), nl.
	
unfill(Command) :-
	write('unfill '), 
	call(Command),
	write(';'), nl.
	
draw(Command) :-
	write('draw '), 
	call(Command),
	write(';'), nl.

rounded_rectangle(X1,Y1,X2,Y2,Rad) :-
	X1R is X1 + Rad, Y1R is Y1 + Rad,
	X2R is X2 - Rad, Y2R is Y2 - Rad,
	write('(quartercircle scaled (u*2*'),
	write(Rad),write(') rotated 180 shifted (u*('),
	write(X1R),write('),u*('),write(Y1R),write('))) -- '),
	write('(quartercircle scaled (u*2*'),
	write(Rad),write(') rotated 270 shifted (u*('),
	write(X2R),write('),u*('),write(Y1R),write('))) -- '),
	write('(quartercircle scaled (u*2*'),
	write(Rad),write(') rotated 0 shifted (u*('),
	write(X2R),write('),u*('),write(Y2R),write('))) -- '),
	write('(quartercircle scaled (u*2*'),
	write(Rad),write(') rotated 90 shifted (u*('),
	write(X1R),write('),u*('),write(Y2R),write('))) -- cycle').

rectangle(X1,Y1,X2,Y2) :-
	write('(u*'),write(X1),write(',u*'),write(Y1),write(') -- '),
	write('(u*'),write(X2),write(',u*'),write(Y1),write(') -- '),
	write('(u*'),write(X2),write(',u*'),write(Y2),write(') -- '),
	write('(u*'),write(X1),write(',u*'),write(Y2),write(') -- cycle').
	
write_islands([]).
write_islands([(OX1,OY1)-(X2,Y2)|Islands]) :-
	X1 is OX1 - 1,
	Y1 is OY1 - 1,
	Rad = 2,
	unfill(rounded_rectangle(X1,Y1,X2,Y2,Rad)),
	write('pickup pencircle scaled (2.5pt);'), nl,
	draw(rounded_rectangle(X1,Y1,X2,Y2,Rad)),
	write('pickup pencircle scaled (1.2pt);'), nl,
	%unfill(rounded_rectangle(X1+0.5,Y1+0.5,X2-0.5,Y2-0.5,Rad)),
	draw(rounded_rectangle(X1+0.5,Y1+0.5,X2-0.5,Y2-0.5,Rad)),
	write_islands(Islands). 

write_walls([],_).
write_walls([(X1,Y1)-(X2,Y2)|Walls],Islands) :-
	( \+(on_island((X1,Y1),Islands)),\+(on_island((X2,Y2),Islands)) ->
		NX1 is X2 - 1, 
		NY1 is Y2 - 1,
		NX2 is NX1 + Y2 - Y1, 
		NY2 is NY1 + X2 - X1,
		write_line(NX1,NY1,NX2,NY2)
	;
		true
	),
	write_walls(Walls,Islands).

write_line(X1,Y1,X2,Y2) :-
	write('draw (u*'), write(X1), write(',u*'),write(Y1),
	write(') -- (u*'), write(X2), write(',u*'),write(Y2),
	write(');'), nl.


