build:
	cabal build
	cp ./dist/build/heapvis/heapvis ./bin/

docs:
	cabal haddock --hyperlink-source --executables

dot-heap:
	bash -c 'imgv <(./bin/heapvis heap | dot -Tpng)'

dot-skew:
	bash -c 'imgv <(./bin/heapvis skew | dot -Tpng)'
