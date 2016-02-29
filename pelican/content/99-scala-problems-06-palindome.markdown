Title: 99 Scala Problems 06 - Find out whether a list is a palindrome
Date: 2015-04-07 09:25:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-06-palindome
Series: "99 Scala Problems"
Summary: 

## The problem

**P06** (*) Find out whether a list is a palindrome.

Example:

``` scala
scala> isPalindrome(List(1, 2, 3, 2, 1))
res0: Boolean = true
```

## Initial thoughts

The algorithm is pretty simple. A list is palindrome when it is equal to its reversed version. So both a procedural and a recursive solution may get a reversed version of the list and then compare the two.

For long lists this could be a performance issue, so a recursive solution could be interesting. It is sufficient to check if head and tail are equal, then remove them and recursively check the remaining list. The last step will either check a list of one element or an empty list (depending on the length of the list being odd or even), which are both palindrome by definition.

## The procedural solution

``` scala
def isPalindrome[A](l: List[A]):Boolean = {
    l == l.reverse
}
```

Nothing special to highlight here.

## The recursive solution

The recursive solution may mimic the procedural implementing a helper function that reverses the list. See [problem 05](/blog/2015/04/07/99-scala-problems-05-reverse/) for an implementation of such a function.

A recursive solution for the palindrome check shall make use of functions to extract the head and tail elements from the list. Since pattern matching cannot separate the tail element, we have to use `List` type methods.

``` scala
def isPalindrome[A](l: List[A]):Boolean = l match {
    case Nil => true
    case List(a) => true
    case list => (list.head == list.last && isPalindrome(list.tail.init))
}
```

The corresponding tail recursive function is

``` scala
def isPalindrome[A](l: List[A]):Boolean = {
    def _palindrome(res: Boolean, rem: List[A]): Boolean = rem match {
        case Nil => res
        case List(a) => res
        case list => _palindrome(res && rem.head == rem.last, rem.tail.init)
    }
    _palindrome(true, l)
}
```

## A custom operator

Scala version 2.10+ define an [extractor](https://issues.scala-lang.org/browse/SI-2575) for pattern matching last element of a list, which is `:+`. The souce code can be found [here](https://github.com/scala/scala/blob/v2.11.6/src/library/scala/collection/SeqExtractors.scala#L5)-

Since I am using Scala 2.9 I want to use this occasion to learn how to implement an operator for the pattern matching.

First of all: I recommend reading the following pages [ Case Classes and Pattern Matching](http://www.artima.com/pins1ed/case-classes-and-pattern-matching.html), [List patterns](http://www.artima.com/pins1ed/working-with-lists.html#16.5), [Extractors](http://www.artima.com/pins1ed/extractors.html). They describe in a very simple and clear way everything you need to know at this point about pattern matching and further clarify what discussed in [problem 05](/blog/2015/04/07/99-scala-problems-05-reverse/) about the `::` class.

The version proposed in Issue 2575 works only on `List` types but is a good starting point for me

``` scala
object :+ {
  def unapply[A] (l: List[A]) = l match {
    case Nil => None
    case _ => Some( (l.init, l.last) )
  }
}
```

This allows to use the `:+` pattern matching operator (namely an _extractor_) to do something like

``` scala
scala> val init :+ tail = List(1,2,3)
init: List[Int] = List(1, 2)
tail: Int = 3
```

As a matter of fact the concept is very simple, even if its implementation is very rich and powerful. The `unapply()` method deconstructs the incoming value returning an `Option`, either `None` if the value does not match the format or `Some()` with a tuple of extracted values.

Following the above example I come up with a first-last extractor `fl()`

``` scala
object fl {
    def unapply[A] (l: List[A]) = l match {
        case Nil => None
        case l => Some(l.head, l.last)
    }
}
```

which works as expected

``` scala
scala> val head fl last = List(1,2,3)
head: Any = 1
last: Any = 3
scala> val head fl last = List(1)
head: Int = 1
last: Int = 1
```

To use it in the `isPalindrome()` recursive function, however, I need something that also returns the list without the first and the last element.

``` scala
object frl {
    def unapply[A] (l: List[A]) = l match {
        case Nil => None
        case l if (l.length == 1) => Some(l.head, l.last, List())
        case l => Some(l.head, l.last, l.init.tail)
    }
}
```

This cannot however be used as an infix operator, because it has three input values, so pattern matching will use the standard call form

``` scala
scala> val frl(first, last, rem) = List(1,2,3,4,5)
first: Int = 1
last: Int = 5
rem: List[Int] = List(2, 3, 4)
```

With this extractor my recursive function is greatly simplified

``` scala
def isPalindrome[A](l: List[A]):Boolean = l match {
    case Nil => true
    case frl(first, last, rem) => (first == last) && isPalindrome(rem)
}
```

and its corresponding tail recursive form is

``` scala
def isPalindrome[A](l: List[A]):Boolean = {
    def _palindrome(res: Boolean, rem: List[A]): Boolean = rem match {
        case Nil => res
        case frl(first, last, rem) => _palindrome(res && first == last, rem)
    }
    _palindrome(true, l)
}
```

## Final considerations

The palindrome problem was straightforward, but this time I had the occasion to understand **pattern matching operators** and **extractors**.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.

