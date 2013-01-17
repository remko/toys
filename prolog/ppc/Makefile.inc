PROLOG = swipl -q
TESTS = $(patsubst %.pl,%.plt,$(SOLUTIONS))
	
.PHONY: all
all:

.PHONY: test_script.pl
test_script.pl: $(SOLUTIONS) $(TESTS)
	( \
		for include in $(SOLUTIONS); do \
			echo ":- ['$$include']."; \
		done; \
		echo ":- use_module('../plunit/plunit.pl')."; \
		echo ":- load_test_files([])."; \
	) > $@

.PHONY: check
check: test_script.pl
	$(PROLOG) -s test_script.pl -g 'run_tests,halt' -t 'halt(1)'

.PHONY: coverage
coverage: test_script.pl
	$(PROLOG) -s test_script.pl -g "use_module('../plunit/test_cover.pl'),show_coverage(run_tests),halt" -t 'halt(1)'
	
.PHONY: clean
clean:
	-rm -rf submission.txt test_script.pl
