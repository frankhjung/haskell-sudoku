#!/usr/bin/env make

SRC	:= $(wildcard app/*.hs src/*.hs test/*.hs)

.PHONY: default
default:check build test

.PHONY: check
check:	tags style lint

.PHONY: all
all:	check build test doc exec

.PHONY: tags
tags:	$(SRC)
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRC)

.PHONY: style
style:	$(SRC)
	@echo style ...
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRC)

.PHONY: lint
lint:	$(SRC)
	@echo lint ...
	@cabal check
	@hlint --cross --color --show $(SRC)

.PHONY: build
build:
	@echo build ...
	@cabal build

.PHONY: test
test:
	@echo test ...
	@cabal test --test-show-details=always

.PHONY: doc
doc:
	@echo doc ...
	@cabal haddock --haddock-quickjump --haddock-hyperlink-source

.PHONY: exec
exec:	$(SRC)
	@echo Easy Puzzle ...
	@echo
	@sed 's/0/-/g' data/easy.sudoku
	@echo
	@echo Easy Solution ...
	@echo
	@cabal exec sudoku -- data/easy.sudoku +RTS -s
	@echo
	@echo Medium Puzzle ...
	@echo
	@sed 's/0/-/g' data/medium.sudoku
	@echo
	@echo Medium Solution ...
	@echo
	@cabal exec sudoku -- data/medium.sudoku +RTS -s
	@echo
	@echo Hard Puzzle ...
	@echo
	@sed 's/0/-/g' data/hard.sudoku
	@echo
	@echo Hard Solution ...
	@echo
	@cabal exec sudoku -- data/hard.sudoku +RTS -s
	@echo
	@echo Expert Puzzle ...
	@echo
	@sed 's/0/-/g' data/expert.sudoku
	@echo
	@echo Expert Solution ...
	@echo
	@cabal exec sudoku -- data/expert.sudoku +RTS -s
	@echo

.PHONY: setup
setup:
	cabal --version
	cabal update --only-dependencies --enable-tests --enable-documentation --enable-benchmarks

.PHONY: clean
clean:
	@cabal clean
	-$(RM) tags
	-$(RM) $(addsuffix .hi, $(basename $(SRC)))
	-$(RM) $(addsuffix .o, $(basename $(SRC)))
	-$(RM) $(addsuffix .prof, $(basename $(SRC)))
