MAIN ?=
ARGS ?=

.DEFAULT_GOAL := autotest
.PHONY: check-main autotest test format run autorun

check-main:
	@if [ -z "$(MAIN)" ]; then \
	  echo "ERROR: MAIN is not set. Usage:" >&2; \
	  echo "  make run MAIN=my.namespace ARGS=\"...\"" >&2; \
	  exit 1; \
	fi

autotest:
	find . -iname '*.clj' | entr make test

test: check-main format
	clj -M:test cognitect.test-runner -n $(MAIN)-test

run: check-main format
	clj -M -m $(MAIN) $(ARGS)

autorun:
	find . -iname '*.clj' | entr make run \
		MAIN=$(MAIN) \
		ARGS="$(ARGS)"

format:
	@echo "Formatting clj."
	standard-clj fix src test deps.edn

%:
	@: