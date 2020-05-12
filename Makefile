build:
	bash -c "mkdir -p src/parser && cd src/parser && rm -rf ./* && bnfc -m ../../declaration.cf && make"
	cabal build
	mv ./dist/build/lelolang-exe/lelolang-exe interpreter
