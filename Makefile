build:
	bash -c "mkdir -p src/parser && rm -rf src/parser/* && bnfc -m -o src/parser declaration.cf && cd src/parser && make"
	cabal build
	mv ./dist/build/lelolang-exe/lelolang-exe interpreter
