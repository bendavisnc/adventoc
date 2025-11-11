MAIN=adventoc.twentytwentyone.twentythree.amphipod
# MAIN ?= adventoc.ancillary.tower-of-hanoi.toh

dev: 
	find . -iname *.clj | entr make run

autotest: 
	find . -iname *.clj | entr make test

alttest: 
	clj -M:test cognitect.test-runner -n adventoc.twentytwentyone.twentythree.amphipod-solution-test

run: format
	# clj -M:dev --main ${MAIN} "true"
	clj -M:dev --main ${MAIN}

test: format
	# clj -M:test cognitect.test-runner -n adventoc.twentytwentyone.twentythree.amphipod-test
	# clj -M:test cognitect.test-runner -n adventoc.ancillary.dijkstra.dijkstra-test
	clj -M:test cognitect.test-runner -n adventoc.twentytwentyone.fifteen.chiton-test

format:
	@echo "Formatting clj."
	standard-clj fix src test deps.edn
