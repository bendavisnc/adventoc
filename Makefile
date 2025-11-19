MAIN ?=

.DEFAULT_GOAL := autotest
.PHONY: check-main autotest test format

check-main:
	@if [ -z "$(MAIN)" ]; then \
	  echo "ERROR: MAIN is not set. Usage:" >&2; \
	  echo "  make test MAIN=my.namespace" >&2; \
	  exit 1; \
	fi

autotest:
	find . -iname '*.clj' | entr make test

test: check-main format
	clj -M:test cognitect.test-runner -n $(MAIN)-test

format:
	@echo "Formatting clj."
	standard-clj fix src test deps.edn
