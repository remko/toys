flag(X) :-
	write_top,
	write_flagtop,
	( X == 1 ->
		write_flagstraight,
		write_flagstraight
	;
		write(' | |'), 
		write_bottom_secondtolast(1, X)
	),
	write(' | |'), 
	write_flagstraightbottom(1, X),
	write(' | |'), 
	write_flagbottom(1, X),
	write_bottom.

%	write_flagswirltop,
%	write_flagswirlbottom,


write_top :-
	write('  _ '), nl,
	write(' (_) '), nl,
	write('<___>'), nl.

write_bottom :-
	write(' | |'), nl.

write_flagtop :-
	write(' | |_____'), nl.

write_flagstraight :-
	write(' | |    |'), nl.


%

write_bottom_secondtolast(I, Edition) :-
	( I == Edition ->
		write('|'),
		N is 3 + I - 1,
		write_character_n(' ', N),
		write('|'),
		nl
	; I is Edition - 1 ->
		N is 4 + I - 1,
		write_character_n('~', N),
		NI is I + 1,
		write_bottom_secondtolast(NI, Edition)
	;
		N is 4 + I - 1,
		write_character_n(' ', N),
		NI is I + 1,
		write_bottom_secondtolast(NI, Edition)
	).
	

write_flagstraightbottom(I, Edition) :-
	( I == Edition ->
		( Edition == 1 ->
			write(' ')
		;
			write('|')
		),
		N is 3 + I - 1,
		write_character_n(' ', N),
		write('|'),
		nl
	;
		N is 4 + I - 1,
		write_character_n(' ', N),
		NI is I + 1,
		write_flagstraightbottom(NI, Edition)
	).

write_flagbottom(I, Edition) :-
	( I == Edition ->
		N is 5 + I - 1,
		write_character_n('~', N),
		nl
	;
		N is 4 + I - 1,
		write_character_n(' ', N),
		NI is I + 1,
		write_flagbottom(NI, Edition)
	).

write_character_n(C, N) :-
	( N == 0 ->
		true
	;
		write(C),
		NN is N - 1,
		write_character_n(C, NN)
	).
