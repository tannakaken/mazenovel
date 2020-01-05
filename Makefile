NOVELDIR := novels
NOVELS := $(wildcard $(NOVELDIR)/novel*.txt)

.PHONY: all
all: main tree

.PHONY: index
main: public/elm.js
public/elm.js: src/Main.elm
	npx elm make src/Main.elm --output public/elm.js

.PHONY: tree
tree: public/tree.json
public/tree.json: $(NOVELS) 
	ruby tree.rb

new:
	perl new.pl

serve:
	php -S localhost:8000 -t public/

.PHONY: test
test:
	npx elm-test

.PHONY: format
format:
	npx elm-format . --yes
