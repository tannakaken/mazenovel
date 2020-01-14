NOVELSDIR := novels
SRCDIR := src
TESTSDIR := tests
NOVELS := $(wildcard $(NOVELSDIR)/novel*.txt)
SRC := $(wildcard $(SRCDIR)/*.elm) 
TESTS := $(wildcard $(TESTSDIR)/*.elm)

.PHONY: all
all: main tree

.PHONY: index
main: public/elm.js
public/elm.js: $(SRC)
	npx elm make src/Main.elm --output public/elm.js

.PHONY: tree
tree: public/tree.json
public/tree.json: $(NOVELS) tree.rb
	ruby tree.rb

new:
	perl new.pl

serve:
	php -S localhost:8000 -t public/

.PHONY: test
test: $(SRC) $(TESTS)
	npx elm-test

.PHONY: format
format:
	npx elm-format . --yes

.PHONY: doc-preview
doc-preview:
	npx elm-doc-preview
