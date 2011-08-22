.PHONY: all test

all: test

test_clean: escalus/ebin
	rm -rf suites/*.beam
	make test

test: escalus/ebin
	rm -rf reports
	mkdir  reports
	erlc -I escalus/deps/exmpp/include run_common_test.erl
	erl -noinput -sname test -setcookie ejabberd \
		-pa `pwd`/suites \
		-pa `pwd`/escalus/ebin \
		`pwd`/escalus/deps/exmpp/ebin -s run_common_test ct

escalus/ebin:
	(cd escalus; make)

console:
	erl -sname test -setcookie ejabberd \
		-pa `pwd`/suites \
		-pa `pwd`/escalus/ebin \
		-pa `pwd`/../tools \
		`pwd`/escalus/deps/exmpp/ebin

.PHONY: escalus/ebin
