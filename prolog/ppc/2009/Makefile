SOLUTIONS = \
	flag.pl \
	split.pl \
	panoz.pl \
	circ.pl \
	bishop.pl 

include ../Makefile.inc

submission.txt: $(SOLUTIONS)
	( \
		echo "%%%%%%%%%%%%%begin submission%%%%%%%%%%%%%"; \
		echo ""; \
		echo "Name: Remko Tronçon"; \
		echo "Prolog system used: SWI-Prolog"; \
		echo ""; \
		for solution in $(SOLUTIONS); do \
			echo "%%%%%%%%%%%%%$$solution%%%%%%%%%%%%%"; \
			echo ""; \
			cat $$solution; \
			echo ""; \
		done; \
		echo "%%%%%%%%%%%%%end submission%%%%%%%%%%%%"; \
	) > $@

