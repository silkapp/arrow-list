#!/bin/sh

tool=Tests
rm ${tool}.tix
mkdir -p dist &&
mkdir -p dist/p &&
ghc-pkg hide hxt &&
ghc --make -O -hide-package=arrow-list -threaded -Wall -i../src -fhpc -rtsopts ${tool}.hs -odir dist   -hidir dist   -o dist/${tool} &&
ghc --make -O -hide-package=arrow-list -prof -auto-all -osuf=op -threaded -Wall -i../src -fhpc -rtsopts ${tool}.hs -odir dist/p -hidir dist/p -o dist/p/${tool}-prof &&
./dist/${tool} $2 $3 $4 # +RTS -N -I2 -qb -qg -RTS
hpc markup ${tool} &&
mv *.html dist
# ./dist/p/${tool}-prof $2 $3 $4 +RTS -p -hy -i0.01 -N -I2 -qb -qg 
