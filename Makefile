MAIN=adventoc.twentytwentyone.twentythree.amphipod
# MAIN ?= adventoc.ancillary.tower-of-hanoi.toh

dev: 
	find . -iname *.clj | entr make run

autotest: 
	find . -iname *.clj | entr make test

run: format
	clj -M:dev --main ${MAIN}

test: format
	clj -M:test

format:
	@echo "Formatting clj."
	standard-clj fix src test deps.edn
