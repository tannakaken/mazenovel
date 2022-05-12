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
	npx elm make src/Main.elm --optimize --output public/elm.js 
	npx uglifyjs public/elm.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | npx uglifyjs --mangle --output public/elm.min.js
	rm public/elm.js

debug: public/elm.js
	npx elm make src/Main.elm --output public/elm.js 

.PHONY: tree
tree: public/tree.json
public/tree.json: $(NOVELS) tree.rb
	ruby tree.rb

new:
	perl new.pl

serve:
	php -S 0.0.0.0:8000 -t public/

.PHONY: test
test: $(SRC) $(TESTS)
	npx elm-test

.PHONY: format
format:
	npx elm-format . --yes

.PHONY: doc-preview
doc-preview:
	npx elm-doc-preview
