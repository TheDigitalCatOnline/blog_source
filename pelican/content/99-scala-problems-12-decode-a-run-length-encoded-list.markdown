Title: 99 Scala Problems 12 - Decode a run-length encoded list
Date: 2015-04-14 11:00:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-12-decode-a-run-length-encoded-list
Series: "99 Scala Problems"
Summary: 

## The problem

**P12** (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version.

Example:

``` scala
scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
```

## Initial thoughts

The problem is simple, being just a reverse version of the last two problems. The target is to build a list from another one, so it should be possible to find both a recursive and a functional solution. The only problem is that we may easily build a list for each element, but we shall end up with a flat list. So `flatMap()` from [problem 07](/blog/2015/04/07/99-scala-problems-07-flatten/) will come in play again.

## The recursive solution

Since I decided to use `flatMap()`, which basically acts like `map()` but then inserts the resulting list into the target list (indeed flattening it), I have to write a function that converts a `(Int, A)` tuple into a `List[A]`, where `A` is a type of choice.

The only issue in building lists in Scala is that the `:::` operator works with two lists, so we have to build a list with the current head to be able to append it.

``` scala
def decode[A](l: List[(Int, A)]):List[A] = {
    def _expand(res: List[A], rem:(Int, A)):List[A] = rem match {
        case (0, _) => res
        case (n, h) => _expand(res:::List(h), (n -1, h))
    }
    
    l flatMap { e => _expand(List(),e) }
}
```

The last call is a mapping. Each element is mapped into a list by the `_expand()` function, while `flatMap()` does the flattening job.

## The functional solution

There is a simpler way that makes use of a function of the `List` object. Object and classes are two different entities in Scala, and objects are singletons that encapsulate constructor (and de-constructor) methods.

The `fill()` method is documented [here](http://www.scala-lang.org/api/2.11.4/index.html#scala.collection.immutable.List$), and we are interested now in the one-dimensional version. As you can see from the documentation, this function is curried (see [problem 04](/blog/2015/04/07/99-scala-problems-04-length/)) and accepts as first parameter the length of the list and as second parameter the element to put into the list itself.

We are just interested in repeating an element this time, so the function invocation is very simple

```scala
def decode2[A](l: List[(Int, A)]):List[A] = {
    l flatMap { e => List.fill(e._1)(e._2) }
}
```

Scala tuples may be indexed using the `_index` notation, starting from 1 (while lists are indexed from 0). Here, just like in the recursive solution, we have to use `flatMap()` to flatten the resulting list.

## Final considerations

Writing this solution I renewed my knowledge of **flatmapping**, learned that Scala classes have **companion objects**, learned how to **create lists** and how to **index tuples**.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
