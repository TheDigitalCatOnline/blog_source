Title: 99 Scala Problems 10 - Run-length encoding of a list.
Date: 2015-04-14 10:00:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-10-run-length-encoding-of-a-list
Series: "99 Scala Problems"
Summary: 

## The problem

**P10** (*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.

Example:

``` scala
scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
```

## Initial thoughts

The problem explicitly states "use the result of problem 09" ([here](/blog/2015/04/07/99-scala-problems-09-pack-consecutive-duplicates/)) so I think it's time for me to learn how to make and import libraries. The problem itself seems to be rather simple to solve.

## The recursive solution

Given the availability of the `pack()` function from problem 09 the recursive solution just has to walk the packed list and convert each element (a list of equal elements) into a tuple. This is the tail recursive version

``` scala
import utils.packer

def encode[A](l: List[A]):List[(Int, A)] = {
    def _encode(res: List[(Int, A)], rem: List[List[A]]):List[(Int, A)] = rem match {
        case Nil => res
        case h::tail => _encode(res:::List((h.length, h.head)), tail)
    }
    _encode(List(), utils.packer.pack(l))
}
```

Note that I use the `utils.packer.pack()` function imported from a package which contains the code developed for problem 09.

To obtain the availability of the `pack()` function I first created a *package*. In Scala a package may be declared with the `package` keyword, but the functions cannot be declared at module-level, they have to be converted into methods of an object. So the content of the `utils.scala` file is

``` scala
package utils

object packer {
    def pack[A](l: List[A]):List[List[A]] = {
        def _pack(res: List[List[A]], rem: List[A]):List[List[A]] = rem match {
            case Nil => res
            case ls => {
                val (s, r) = rem span { _ == rem.head }
                _pack(res:::List(s), r)
            }
        }
        _pack(List(), l)
    }
}
```

In Scala, an `object` is a singleton, and it is used without instancing it. So the `pack()` method may be used in this file as `packer.pack()`. This file is then compiled with the Scala compiler

``` bash
$ scalac utils.scala
```

producing a `utils` directory with the following files

``` bash
$ ls utils
packer$$anonfun$1.class
packer.class
packer$.class
```

This package may be directly used by any file in the same directory (e.g. through `scala program.scala`). If you plan to use it from another directory use the `-classpath` switch to include the right directory where the `utils` package may be found. (As an UNIX programmer, I hate single-dash Java long options, but they are here to stay)

## Mapping

Mapping is, just like folding, a _functional_ technique, because it applies a function to the elements of some collection. In Scala, the `map()` function of the `List` type produces a new list processing each element of the original list with the given function.

The function to process elements is very simple, since it has to pack together the length of the element (which is a list) and its head.

``` scala
import utils.packer

def encode[A](l: List[A]):List[(Int, A)] = {
    utils.packer.pack(l) map { e => (e.length, e.head) }
}
```

The expression `e => (e.length, e.head)` is an anonymous function that maps an element into a tuple.

## Final considerations

With this problem I met **packages** for the first time, learned how to **compile** and add paths through **classpath**. The functional solution introduced me to **mapping**.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
