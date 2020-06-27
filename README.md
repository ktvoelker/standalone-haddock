standalone-haddock
==================

> :rocket: **New maintainer wanted!** It's not much work, but I don't use this software very
> often. I think a regular user would make a better maintainer. Please email karl@karlv.net
> if you are interested.

standalone-haddock generates standalone haddock Haskell documentation.

Note: you need this program to publish haddocks on your own website. If you want
to upload haddocks to hackage instead, follow [these instructions][for-hackage].

[for-hackage]: http://fuuzetsu.co.uk/blog/posts/2014-01-06-Hackage-documentation-v2.html

When you simply run `cabal haddock`, the resulting HTML documentation contains
hyperlinks to other packages on your system. As a result, you cannot publish it
on the internet (well, you can, but the links will be broken).

standalone-haddock takes several packages for which you want to publish
documentation. It generates documentation for them with proper links:

* links to identifiers inside this package set are relative
* links to identifiers from external packages lead to hackage

Thus the resulting directory with HTML files is relocatable and publishable.

**TL;DR**: it just works. See the [haskell-suite][] documentation for an example
output.

[haskell-suite]: http://haskell-suite.github.io/docs

Warning
-------

Using Haddock 2.16.0 (the version which is bundled with GHC 7.10.1) will result
in broken links in `index.html`. Please use any other version of Haddock.

Usage
-----

    Usage: standalone-haddock [--package-db DB-PATH] -o OUTPUT-PATH [PACKAGE-PATH]

    Available options:
      -h,--help                Show this help text
      --package-db DB-PATH     Additional package database
      -o OUTPUT-PATH           Directory where html files will be placed

`PACKAGE-PATH` is the path to the (unpacked) package — i.e. a directory with a
`.cabal` file.

For example:

    standalone-haddock -o doc haskell-names haskell-packages haskell-src-exts hse-cpp cabal/Cabal

**NOTE**: dependencies of every package need to be already installed in the
system with documentation (even those dependencies that themselves belong to the
current package set).

By default, only the global database is read. Other databases can be included
with the `--package-db` option. For example, to include the standard local
database:

    standalone-haddock -o doc --package-db $HOME/.ghc/i386-linux-7.6.3/package.conf.d pkg1 pkg2

(replace `i386-linux-7.6.3` with your platform id).

### Stack

To use with `stack`, go to the directory of your package and enter:

```sh
standalone-haddock -o docs \
  --compiler-exe=$(stack path --compiler-exe) \
  --dist-dir=$(stack path --dist-dir) \
  --package-db=$(stack path --snapshot-pkg-db) \
  --package-db=$(stack path --local-pkg-db) .
```
