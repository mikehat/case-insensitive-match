
package = case-insensitive-match
lib = dist/build/libHS$(package)-*
lib-hs-files = $(shell find lib/ -type f -name \*.hs)



.PHONY : all

all : $(package) tests benchmarks docs readme-example


.PHONY : configure build test

configure : dist/setup-config

dist/setup-config :
	cabal configure --enable-tests --enable-benchmarks

build :
	cabal build

test :
	cabal test



.PHONY : $(package)

$(package) : $(lib).a

$(lib).a : dist/setup-config $(lib-hs-files)
	cabal build $(package)


.PHONY : tests test-basics

tests : dist/build/test-basics/test-basics

test-basics : dist/build/test-basics/test-basics

dist/build/test-basics/test-basics : $(lib).a src/test-basics.hs
	cabal build test-basics



.PHONY : benchmarks

benchmarks : dist/build/bench-others/bench-others dist/build/bench-tagsoup/bench-tagsoup

dist/build/bench-others/bench-others : $(lib).a src/bench-others.hs
	cabal build bench-others

dist/build/bench-tagsoup/bench-tagsoup : $(lib).a src/bench-tagsoup.hs
	cabal build bench-tagsoup



.PHONY : readme-example

readme-example : dist/build/readme-example/readme-example

dist/build/readme-example/readme-example : $(lib).a src/readme-example.hs
	cabal build readme-example



.PHONY : docs

docs : dist/doc/html/case-insensitive-match/index.html

dist/doc/html/case-insensitive-match/index.html : $(lib-hs-files)
	cabal haddock




.PHONY : clean clean-all

clean :
	cabal clean

clean-all : clean
