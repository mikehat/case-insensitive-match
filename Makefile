
package = case-insensitive-match
lib = dist/build/libHS$(package)-*




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

$(lib).a : dist/setup-config $(shell find lib/ -type f -name \*.hs)
	cabal build $(package)


.PHONY : tests test-basics

tests : dist/build/test-basics/test-basics

test-basics : dist/build/test-basics/test-basics

dist/build/test-basics/test-basics : $(lib).a src/test-basics.hs
	cabal build test-basics



.PHONY : benchmarks

benchmarks : dist/build/bench-others/bench-others

dist/build/bench-others/bench-others : $(lib).a src/bench-others.hs
	cabal build bench-others



.PHONY : readme-example

readme-example : dist/build/readme-example/readme-example

dist/build/readme-example/readme-example : $(lib).a src/readme-example.hs
	cabal build readme-example



.PHONY : docs

docs : dist/doc/html

dist/doc/html : lib/*/*.hs
	cabal haddock




.PHONY : clean clean-all

clean :
	cabal clean

clean-all : clean
