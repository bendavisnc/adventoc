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
	find . -iname '*.clj' | entr env MAIN=$(MAIN) ARGS="$(ARGS)" make run

format:
	@echo "Formatting Clojure for $$MAIN with zprint..."
	@MAIN_PATH="$$(echo "$$MAIN" | sed 's/\.[^.]*$$//' | tr . /)"; \
	echo "Computed MAIN_PATH (directory): $$MAIN_PATH"; \
	DIRS=""; \
	[ -e "src/$$MAIN_PATH" ]  && DIRS="$$DIRS src/$$MAIN_PATH"; \
	[ -e "test/$$MAIN_PATH" ] && DIRS="$$DIRS test/$$MAIN_PATH"; \
	if [ -n "$$DIRS" ]; then \
	  echo "Formatting files under: $$DIRS"; \
	  find $$DIRS \( -iname "*.clj" \) -print0 \
	    | xargs -0 zprint '{:style [:community :respect-nl :justified] }' -w; \
	else \
	  echo "No matching src/test paths for $$MAIN"; \
	fi


%:
	@: