NOVELDIR := novels
NOVELS := $(wildcard $(NOVELDIR)/novel*.txt)

.PHONY: all
all: index tree

.PHONY: index
index: public/index.html
public/index.html: src/Main.elm
	npx elm make src/Main.elm --output public/index.html

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
