# random-access-map

[![Build Status](https://travis-ci.org/djhaskin987/random-access-map.png)](https://travis-ci.org/djhaskin987/random-access-map)

A Clojure library provides the random-access-map, a set which allows you to retreive
and cut elements from the set based on the magnitude of their rank.

## Attribution
_People whose ideas I stole_

Matt Might has a [_great_ article](http://matt.might.net/articles/red-black-delete/)
on how to perform functional red-black tree removal, without which this would
be a much lesser data structure.

The [Clojure Cookbook's article on red black trees](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/02_composite-data/2-27_and_2-28_custom-data-structures/2-27_red-black-trees-part-i.asciidoc#sec_red_black_part_ii) was invaluable in making this data structure.

## Build

Simply run `lein jar` to get the jar file.

## Usage

This map implements the `clojure.lang.IPersistentMap` protocol, as well as the `clojure.lang.Indexed` protocol.

## License

Copyright © 2013-2014 Daniel Jay Haskin.

Distributed under the Eclipse Public License, the same as Clojure.
