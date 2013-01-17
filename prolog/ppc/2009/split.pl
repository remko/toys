split(Large, Small1, Small2, How) :-
	findall(L-H, (split_nondet(Large, Small1, Small2, H), split_length(H,L,-)), Hows),
	sort(Hows, SHows),
	SHows = [_-How|_].

split_nondet([], [], [], []).
split_nondet([L|Ls], Small1, Small2, [H|Hs]) :-
	Small1 = [L|S1s],
	H = 1,
	split_nondet(Ls, S1s, Small2, Hs).
split_nondet([L|Ls], Small1, Small2, [H|Hs]) :-
	Small2 = [L|S2s],
	H = 2,
	split_nondet(Ls, Small1, S2s, Hs).

split_length([], 0, _).
split_length([H|Hs], L, Prev) :-
	split_length(Hs, L1, H),
	( H == Prev ->
		L = L1
	;
		L is L1 + 1
	).
