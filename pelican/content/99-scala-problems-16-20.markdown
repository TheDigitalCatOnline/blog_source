Title: 99 Scala Problems 16-20
Date: 2015-05-13 16:00:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-16-20
Series: "99 Scala Problems"
Summary: 

# Problem 16

## The problem

**P16** (**) Drop every Nth element from a list.

Example:

``` scala
scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
```

## Mapping

A first simple approach is to divide the list in small groups of up to N elements and drop the last element from each of them. This is easily achieved with the `grouped()` method of `List` types and the application of a flat mapping.

``` scala
def drop[A](n: Int, l: List[A]):List[A] = {
    l.grouped(n).flatMap { _.take(n - 1) }.toList
}
```

The `grouped()` method returns an iterator, which is, as in other languages, something that can visit the elements contained in a collection and yield them. When an iterator is required to be consumed all at once you can call the `toList()` method to convert it into a `List`.

``` scala
scala> val l = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
l: List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

scala> l.grouped(3)
res0: Iterator[List[Symbol]] = non-empty iterator

scala> l.grouped(3).toList
res1: List[List[Symbol]] = List(List('a, 'b, 'c), List('d, 'e, 'f), List('g, 'h, 'i), List('j, 'k))
```

`Iterator` type has [a lot of methods](http://www.scala-lang.org/api/2.11.5/index.html#scala.collection.Iterator) in common with `List` type, most notably `flatMap()`. This last method is fed with a simple anonymous function that, using `take()`, keeps the first `n - 1` elements.

A different approach may be followed using the `zipWithIndex()` method that packs each element of the list with its index (starting from 0)

``` scala
scala> l zipWithIndex
res5: List[(Symbol, Int)] = List(('a,0), ('b,1), ('c,2), ('d,3), ('e,4), ('f,5), ('g,6), ('h,7), ('i,8), ('j,9), ('k,10))
```

After this transformation, we may apply the `filter()` method to get the right elements with a selection based on the modulo operation.

The list is now made of `Tuple2` elements, as tuples in Scala aren't a collection just like lists (or like Python tuples). For performance reasons, they are implemented with a different type according to the length, like in this case. To index a `Tuple2` element we may use its `_1` and `_2` attributes (pay attention that tuples are indexed starting from 1).

The filter anonymous function just checks if the index (the second element of the tuple) is a multiple of `n`. Then, with `map()` we keep the first element only (the original element); the anonymous function `{ _._1 }` is a shortcut form for `{ e => e._1 }`

``` scala
def dropZip[A](n: Int, l: List[A]):List[A] = {
    l.zipWithIndex filter { e => (e._2 + 1) % n != 0 } map { _._1 }
}
```

## The recursive solution

A recursive solution can be written based on this simple algorithm: the number N is used as a countdown, keeping elements until it reaches the value 1. Then the current element is discarded and the process continues with a countdown reset.

``` scala
def dropRec[A](n: Int, l: List[A]):List[A] = {
    def _dropRec[A](c: Int, res: List[A], rem: List[A]):List[A] = (c, rem) match {
        case (_, Nil) => res
        case (1, _::tail) => _dropRec(n, res, tail)
        case (_, h::tail) => _dropRec(c -1, res:::List(h), tail)
    }
    _dropRec(n, List(), l)
}
```

As you can see the case `(1, _::tail)` restarts the process ignoring the current head of the list, while the more generic `(_, h::tail)` case stores the element in the result list.

# Problem 17

## The problem

**P17** (*) Split a list into two parts.

Example:

``` scala
scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
```

## Solution

`List` objects provide a bunch of methods to interact with them, thus splitting may make use of several different ones. Very simple and straightforward solutions come from the `splitAt()` method

``` scala
def split[A](n: Int, l: List[A]): (List[A], List[A]) = l.splitAt(n)
```

using the two `take()` and `drop()` methods

``` scala
def split[A](n: Int, l: List[A]): (List[A], List[A]) = {
    (l.take(n), l.drop(n))
}
```

or using `take()` and `takeRight()`

``` scala
def split[A](n: Int, l: List[A]):(List[A], List[A]) = {
    (l.take(n), l.takeRight(l.length - n))
}
```

A list can be splitted also using recursion, however, using the given split position as a counter

``` scala
def split[A](n: Int, l: List[A]):(List[A], List[A]) = {
    def _split[A](c: Int, res: List[A], rem: List[A]):(List[A],List[A]) = (c, rem) match {
        case (_, Nil) => (res, Nil)
        case (0, rem) => (res, rem)
        case (c, h::tail) => _split(c - 1, res:::(List(h)), tail)
    }
    _split(n, List(), l)
}
```

where I basically keep extracting the head from `rem` appending it to `res` until the counter is 0.

# Problem 18

## The problem

**Pxx** (**) Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list.  Start counting the elements with 0.

Example:

``` scala
scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g)
```

## Solution

As for the previous problem we may make use of the `List` type methods. The aptly named `slice()`

``` scala
def slice[A](start: Int, end: Int, l: List[A]): List[A] =
    l.slice(start, end)
```

or a combination of `drop()` and `take()`

``` scala
def slice[A](i: Int, k: Int, l: List[A]):List[A] = {
    l take k drop i
}
```

The recursive solution first drops the required number of elements from the head of the list. When the first index is exhausted the function starts dropping elements from the tail (using `init()`). The number of elements to be dropped is computed as `k - i` when the internal function is called.

``` scala
def slice[A](i: Int, k: Int, l: List[A]):List[A] = {
    def _slice[A](cl: Int, cr: Int, rem: List[A]):List[A] = (cl, cr, rem) match {
        case (0, 0, rem) => rem
        case (0, cr, rem) => _slice(0, cr - 1, rem.init)
        case (cl, cr, h::rem) => _slice(cl - 1, cr, rem)
    }
    _slice(i, k - i, l)
}
```

# Problem 19

## The problem

**P19** (**) Rotate a list N places to the left.

Example:

``` scala
scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
```

## Solution

Rotating a list has some caveats. First one must consider wrapping, so the first thing I did is to calculate the actual shift with the modulo operation. The second thing is that rotation can happen in both directions, and the case of negative shift can be converted to a positive one just adding the shift to the length of the list (being the shift negative this is actually a subtraction). Eventually we can perform the rotation which requires a composition of the output of `drop()` and `take()`

``` scala
def rotate[A](n: Int, l: List[A]):List[A] = {
    val wrapn = if (l.isEmpty) 0 else n % l.length
    if (wrapn < 0) rotate(l.length + n, l)
    else l.drop(wrapn):::l.take(wrapn)
}
```

# Problem 20

## The problem

**P20** (*) Remove the Kth element from a list.
Return the list and the removed element in a Tuple.  Elements are numbered from 0.

``` scala
scala> removeAt(1, List('a, 'b, 'c, 'd))
res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
```

## Solution

There are two edge conditions in this problem. The first is when the list is empty, and the second is when we are asking for an index outside the list. Since an empty list has length 0 both conditions can be summarized checking if the requested index is greater or equal than the length of the list (the "or equal" part comes from the fact that lists are indexed starting from 0).

The simplest solution uses `take()` and `drop()`

``` scala
def removeAt[A](n: Int, l: List[A]):(List[A], A) = {
    if (l.length <= n ) throw new NoSuchElementException
    (l.take(n):::l.drop(n).tail, l(n))
}
```

The problem with boundaries comes from the use of `tail()`, that cannot be applied to an empty list, while `take()` and `drop()` seamlessly work on empty lists and negative indexes. So using the `take()` counterpart `takeRight()` we could write

``` scala
def removeAt[A](n: Int, l: List[A]):(List[A], A) = {
    (l.take(n):::l.takeRight(l.length - n), l(n))
}
```

which automatically raises an `IndexOutOfBoundsException` when the requested index is outside the list. If we want a `NoSuchElementException`, however, we have to wrap the code with a `try` statement.

We can also use mapping, even if in this case its use is probably overkill

``` scala
def removeAt[A](n: Int, l: List[A]):(List[A], A) = {
    (l.zipWithIndex filter { e => e._2 != n } map { _._1 }, l(n))
}
```

this solution raises an `IndexOutOfBoundsException` when the index is outside the list, just like the previous one.

A recursive solution may use the index as a countdown value to get elements until the requested one pops up, then skipping it and getting the rest of the list.

``` scala
def removeAtR[A](n: Int, l: List[A]):(List[A], A) = {
    def _removeAtR[A](k: Int, res: List[A], rem: List[A]):(List[A], A) = (k, rem) match {
        case (0, h::tail) => return (res:::tail, h)
        case (k, Nil) => throw new NoSuchElementException
        case (k, h::tail) => return _removeAtR(k - 1, res:::List(h), tail)
    }
    return _removeAtR(n, List(), l)
}
```


## Final considerations

Problem 16 introduced me to new **list methods** `filter()` and `zipWithIndex()`. I met **iterators** and **tuples** for the first time and learned how to deal with them on a basic level.  Problem 17 introduced the new `splitAt()` method and let me review `take()`, `takeRight()` and `drop()`. The recursive solution makes use of everything has been already discovered solving the previous problems. With problem 18 I learned a new method of the `List` type, `slice()`. The recursive solution is pretty straightforward but matching a tuple of three values requires some attention. I appreciate the **syntactic sugar** that allows to drop parenthesis when calling a function with a single argument, it makes the call resemble natural language. Problems 19 and 20 are just applications of the same methods.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
