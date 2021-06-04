#!/usr/bin/env make

SRC	:= $(wildcard app/*.hs src/*.hs test/*.hs)

.PHONY: default
default:check build test

.PHONY: check
check:	tags style lint

.PHONY: all
all:	check build test doc bench exec

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

.PHONY: bench
bench:
	@echo bench ...
	@cabal bench

.PHONY: doc
doc:
	@echo doc ...
	@cabal haddock --haddock-quickjump --haddock-hyperlink-source

.PHONY: exec
exec:	$(SRC)
	@echo Easy Puzzle ...
	@echo
	@sed 's/0/-/g' data/easy.puzzle
	@echo
	@echo Easy Solution ...
	@echo
	@cabal exec sudoku -- data/easy.puzzle +RTS -s
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
