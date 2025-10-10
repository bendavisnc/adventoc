
dev: 
	find . -iname *.clj | entr make run

run: format
	clj -M --main adventoc.twentytwentyone.twentythree.amphipod

format:
	@echo "Formatting clj."
	standard-clj fix src
