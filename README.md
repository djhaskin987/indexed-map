# indexed-map

[![Build Status](https://travis-ci.org/djhaskin987/indexed-map.png)](https://travis-ci.org/djhaskin987/indexed-map)

A Clojure library that provides the indexed-map, a set which allows retrieval and
removal from a sorted map using the index of the element in logarithmic time.

## Attribution
_People Whose Ideas I Stole_

Matt Might has a [_great_ article](http://matt.might.net/articles/red-black-delete/)
on how to perform functional red-black tree removal, without which this would
be a much lesser data structure.

The [Clojure Cookbook's article on red black trees](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/02_composite-data/2-27_and_2-28_custom-data-structures/2-27_red-black-trees-part-i.asciidoc#sec_red_black_part_ii) was invaluable in making this data structure.

Chris Okasaki's _Functional Data Structures_, Chapter 2, is a must-read for anyone who wants to learn red-black trees.

## Build

Simply run `lein jar` to get the jar file.

## Usage

This map implements the `clojure.lang.IPersistentMap` protocol, as well as the `clojure.lang.Indexed` protocol.
One additional function, `disjoin-nth`, is implemented, which takes an index associated with a
key/value pair in the map and removes that pair.

## License

Copyright © 2013-2014 Daniel Jay Haskin.

Distributed under the Eclipse Public License, the same as Clojure.