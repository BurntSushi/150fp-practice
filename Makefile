build:
	cabal build
	cp ./dist/build/heapvis/heapvis ./bin/

clean:
	rm -rf dist
	cabal configure

docs:
	cabal haddock --hyperlink-source --executables
	rscp ./dist/doc/html/150fp-practice Geils:~/www/burntsushi.net/public_html/docs/haddock

dot-heap:
	bash -c 'imgv -increment 100 <(./bin/heapvis heap | dot -Tpng)'

dot-skew:
	bash -c 'imgv -increment 100 <(./bin/heapvis skew | dot -Tpng)'

push:
	git push origin master
	git push github master
