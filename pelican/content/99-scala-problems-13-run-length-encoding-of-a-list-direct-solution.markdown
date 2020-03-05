Title: 99 Scala Problems 13 - Run-length encoding of a list (direct solution)
Date: 2015-04-14 11:30:00 +0100
Modified: 2019-02-28 10:00:00 +0000
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-13-run-length-encoding-of-a-list-direct-solution
Series: 99 Scala Problems
Image: 99-scala-problems
Summary: Discussing how to process a whole list through the run-length encoding in Scala

# The problem

**P13** (**) Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.

Example:

``` scala
scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
```

# Initial thoughts

Obviously the issue shouldn't be solved just copying all stuff from [problem 09]({filename}99-scala-problems-09-pack-consecutive-duplicates.markdown) and [problem 10]({filename}99-scala-problems-10-run-length-encoding-of-a-list.markdown) inside a single file. There shall be a way to build a single-function solution.

# Spanning

Scala lists provide a `span()` method that splits the list in two. It scans all elements in order, storing them into a list while the given predicate (a function) is true. When the predicate is false `span()` stops and returns the two resulting lists.

It seems to be the perfect candidate for our job. We just have to recursively apply it obtaining two lists. The first list is made by equal elements, and can be reduced to a tuple, the second list is given to the recursive call.

``` scala
def encodeDirect[A](l: List[A]):List[(Int, A)] = {
    def _encodeDirect(res: List[(Int, A)], rem: List[A]):List[(Int, A)] = rem match {
        case Nil => res
        case ls => {
            val (s, r) = rem span { _ == rem.head }
            _encodeDirect(res:::List((s.length, s.head)), r)
        }
    }
    _encodeDirect(List(), l)
} 
```

The predicate given to `span()` is `_ == rem.head`, an anonymous function that tests if the current element is equal to the first element. While this is always true for the first element, by definition, other elements can be equal or not, and the job of `span()` is to find where to split the list.

So example given in the problem runs through the following steps

``` scala
scala> var rem = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
rem: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

scala> var (res, rem1) = rem span { _ == rem.head }
res: List[Symbol] = List('a, 'a, 'a, 'a)
rem1: List[Symbol] = List('b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

scala> var (res, rem2) = rem1 span { _ == rem1.head }
res: List[Symbol] = List('b)
rem2: List[Symbol] = List('c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

scala> var (res, rem3) = rem2 span { _ == rem2.head }
res: List[Symbol] = List('c, 'c)
rem3: List[Symbol] = List('a, 'a, 'd, 'e, 'e, 'e, 'e)

scala> var (res, rem4) = rem3 span { _ == rem3.head }
res: List[Symbol] = List('a, 'a)
rem4: List[Symbol] = List('d, 'e, 'e, 'e, 'e)

scala> var (res, rem5) = rem4 span { _ == rem4.head }
res: List[Symbol] = List('d)
rem5: List[Symbol] = List('e, 'e, 'e, 'e)

scala> var (res, rem6) = rem5 span { _ == rem5.head }
res: List[Symbol] = List('e, 'e, 'e, 'e)
rem6: List[Symbol] = List()
```

# Final considerations

I learned another very useful method to **split** Scala lists, ``span()``. I also used a **complex expression** in a case statement, as already done in problems 09 and 10.

# Feedback

The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.
