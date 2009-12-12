runtests : Test
	ghc -O2 -fexcess-precision -funfolding-use-threshold=48 -threaded --make Test.hs
	./Test -j 4 --test-seed=random --maximum-generated-tests=1001 --maximum-unsuitable-generated-tests=10000

build : Main
	ghc -O2 -fexcess-precision -funfolding-use-threshold=48 --make Main.hs


buildprofilable : Main
  ghc -O2 -fexcess-precision -funfolding-use-threshold=48 --make Main.hs -prof -auto-all -caf-all -fforce-recomp