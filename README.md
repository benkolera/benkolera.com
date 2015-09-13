# Blog

This is the hakyll based blog of Ben Kolera

## Setup

It requires some haskell & nodejs dependencies to operate:

- ghc
- cabal-install
- nodejs
- bower
- livereload
- Livereload chrome plugin

So long as you have all of these already, getting this up and running is
as easy as:

```sh
cabal sandbox init
cabal install --only-dependencies
bower install
npm install
# And then run these two things:
cabal run watch
node livereload.js
```
