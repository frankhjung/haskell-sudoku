#!/usr/bin/env make

TARGET	:= sudoku

.DEFAULT_GOAL := default
SRC	:= $(wildcard app/*.hs src/*.hs test/*.hs)

.PHONY: default
default:format check build test

.PHONY: all
all:	format check build test doc exec

.PHONY: format
format:
	@stylish-haskell --inplace $(SRC)
	@cabal-fmt --inplace sudoku.cabal

.PHONY: check
check:	tags lint

.PHONY: tags
tags:
	@hasktags --ctags --extendedctag $(SRC)

.PHONY: lint
lint:
	@cabal check --verbose=3
	@hlint --cross --color --show $(SRC)

.PHONY: build
build:
	@stack build --pedantic

.PHONY: test
test:
	@stack test

.PHONY: doc
doc:
	@stack haddock

.PHONY: exec
exec:	$(SRC)
	@echo Easy Puzzle ...
	@echo
	@sed 's/0/-/g' data/easy.sudoku
	@echo
	@echo Easy Solution ...
	@echo
	@stack exec $(TARGET) -- data/easy.sudoku +RTS -s
	@echo
	@echo Medium Puzzle ...
	@echo
	@sed 's/0/-/g' data/medium.sudoku
	@echo
	@echo Medium Solution ...
	@echo
	@stack exec $(TARGET) -- data/medium.sudoku +RTS -s
	@echo
	@echo Hard Puzzle ...
	@echo
	@sed 's/0/-/g' data/hard.sudoku
	@echo
	@echo Hard Solution ...
	@echo
	@stack exec $(TARGET) -- data/hard.sudoku +RTS -s
	@echo
	@echo Expert Puzzle ...
	@echo
	@sed 's/0/-/g' data/expert.sudoku
	@echo
	@echo Expert Solution ...
	@echo
	@stack exec $(TARGET) -- data/expert.sudoku +RTS -s
	@echo

.PHONY: setup
setup:
	stack update
	stack path
	stack query
	stack ls dependencies

.PHONY: ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults

.PHONY: clean
clean:
	@cabal clean
	@stack clean

.PHONY: cleanall
cleanall: clean
	@stack purge
	@$(RM) tags
