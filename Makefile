NOVELDIR := novels
NOVELS := $(wildcard $(NOVELDIR)/novel*.txt)

.PHONY: all
all: index tree

.PHONY: index
index: public/index.html
public/index.html: src/Hello.elm
	elm make src/Hello.elm --output public/index.html

.PHONY: tree
tree: public/tree.json
public/tree.json: $(NOVELS) 
	ruby tree.rb

new:
	perl new.pl

