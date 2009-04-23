all: lambdascape

lambdascape:
	ghc --make Main.hs -o lambdascape

clean:
	rm -f lambdascape
