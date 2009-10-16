APPLICATION := parse_trans
APP_FILE := ebin/$(APPLICATION).app
SOURCES := $(wildcard src/*.erl)
HEADERS := $(wildcard src/*.hrl)
MODULES := $(patsubst src/%.erl,%,$(SOURCES))
BEAMS := $(patsubst %,ebin/%.beam,$(MODULES))

comma := ,
e :=
space := $(e) $(e)
MODULELIST := $(subst $(space),$(comma),$(MODULES))

TEST_SOURCES := $(wildcard test/*.erl)
TEST_BEAMS := $(patsubst %.erl,%.beam, $(TEST_SOURCES))

EXAMPLE_SOURCES := $(wildcard examples/*.erl)
EXAMPLE_BEAMS := $(patsubst %.erl,%.beam, $(EXAMPLE_SOURCES))

UTIL_SOURCES := $(wildcard util/*.erl)
UTIL_BEAMS := $(patsubst %.erl,%.beam, $(UTIL_SOURCES))

include vsn.mk

.PHONY: all clean dialyzer

all: $(APPLICATION) doc

$(APPLICATION): $(BEAMS) $(APP_FILE)

test: $(APPLICATION) $(TEST_BEAMS) util/run_test.beam
	@echo Running tests
	@erl -pa util/ -pa ebin/ -pa test/ -noinput -s run_test run

test/%.beam: test/%.erl
	@echo Compiling $<
	@erlc +debug_info -o test/ $<

examples: $(EXAMPLE_BEAMS)

examples/%.beam: examples/%.erl
	@echo Compiling $<
	@erlc -pa ebin -pa examples +debug_info -o examples/ $<

$(APP_FILE): src/$(APPLICATION).app.src
	@echo Generating $@
	@sed -e 's/@MODULES@/$(MODULELIST)/' -e 's/@VSN@/$(VSN)/' $< > $@

ebin/%.beam: src/%.erl $(HEADERS) $(filter-out $(wildcard ebin), ebin)
	@echo Compiling $<
	@erlc +debug_info +warn_missing_spec -o ebin/ $<

ebin:
	@echo Creating ebin/
	@mkdir ebin/

doc: doc/edoc-info util

dialyzer: util/my_plt.plt
	@echo Running dialyzer on sources
	@dialyzer --src -r src/ --plt util/my_plt.plt

doc/edoc-info: doc/overview.edoc $(SOURCES) 
	@erlc -o util/ util/make_doc.erl
	@echo Generating documentation from edoc
	@erl -pa util/ -noinput -s make_doc edoc

util: $(UTIL_BEAMS)

util/%.beam: util/%.erl
	@erlc -o util/ -DTHIS_APP=$(APPLICATION) $<

util/my_plt.plt: util/make_plt.beam
	@erl -noinput -pa util -eval 'make_plt:add([syntax_tools],"util/my_plt.plt")'

clean:
	@echo Cleaning
	@rm -f ebin/*.{beam,app} test/*.beam doc/*.{html,css,png} doc/edoc-info
	@rm -r cover_report
	@rm -f util/*.beam

release: clean all test dialyzer
	@util/releaser $(APPLICATION) $(VSN)
