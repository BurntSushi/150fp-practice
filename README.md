### Usage

```bash
git clone git://github.com/BurntSushi/150fp-practice
cabal update
cabal configure
cabal install --only-dependencies
cabal build
```

You should then be able to get Dot output for a regular `heap` or a `skew` 
heap. In order to visualize it, you should pipe it to the `dot` program:

```bash
./dist/build/heapvis/heapvis skew | dot -Tpdf > skew.pdf
./dist/build/heapvis/heapvis heap | dot -Tpdf > heap.pdf
```

Alternatively, visualize [ntenczar](https://github.com/ntenczar)'s colorless 
Red-Black tree:

```bash
./dist/build/heapvis/heapvis rb | dot -Tpdf > rb.pdf
```


### Documentation

Library documentation:
http://burntsushi.net/docs/haddock/150fp-practice/index.html

`heapvis` documentation: 
http://burntsushi.net/docs/haddock/150fp-practice/heapvis/index.html

