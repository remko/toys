:- begin_tests(split).

test(split_example, [true(How == [2, 1, 1, 1, 2, 2, 2, 1])]) :-
	split([i,i,c,l,c,f,p,p], [i,c,l,p], [i,c,f,p], How).
	
:- end_tests(split).
