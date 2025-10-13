# MAIN=adventoc.twentytwentyone.twentythree.amphipod
MAIN=adventoc.ancillary.tower-of-hanoi.toh

dev: 
	find . -iname *.clj | entr make run

run: format
	clj -M --main ${MAIN}

format:
	@echo "Formatting clj."
	standard-clj fix src
