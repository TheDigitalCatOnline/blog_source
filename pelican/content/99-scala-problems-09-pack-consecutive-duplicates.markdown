Title: 99 Scala Problems 09 - Pack consecutive duplicates of list elements into sublists
Date: 2015-04-07 09:40:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-09-pack-consecutive-duplicates
Series: "99 Scala Problems"
Summary: 

## The problem

**P09** (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

Example:

``` scala
scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
```

## Initial thoughts

This is similar to [problem 08](/blog/2015/04/07/99-scala-problems-08-eliminate-consecutive-duplicates/) but has an important difference: the result will be a list of lists. When dealing with multi-level structures like these, it is very easy to get lost, so I expect to find a recursive solution with a lot of cases or complex pattern guards.

## The recursive solution

The tail recursive solution is easier to write than the standard recursive one. The resulting list we are building is a `List[List[A]]`, while the remainder is just a plain list `List[A]`.

The first case is straightforward. The second case extracts an element from the list and, if the result is empty of the result doesn't contain that element, appends a new list. Pay attention: we have to check `res.last.head` since `res` is a list of lists. So `res.last` is the last list we put into `res` and its head is the repeated element. Obviously the same thing is accomplished by checking `res.last.last`, since this algorithm packs together equal values. The third case covers the situation in which the extracted element is already in the last list or `res`, so we have to modify this latter. To do this we separate `res` in `res.init` and `res.last`, append `h` to `res.last` (`res.last:::List(h)`) and merge again the two pieces.

``` scala
def pack[A](l: List[A]):List[List[A]] = {
    def _pack(res: List[List[A]], rem: List[A]):List[List[A]] = rem match {
        case Nil => res
        case h::tail if (res.isEmpty || res.last.head != h) => _pack(res:::List(List(h)), tail)
        case h::tail => _pack(res.init:::List(res.last:::List(h)), tail)
    }
    _pack(List(),l)
}
```

A very simpler solution, however, comes from the `span()` method of `List` objects. This method "splits this list into a prefix/suffix pair according to a predicate."

In other words, `span()` selects the elements of the list (preserving order) that satisfy a given function until it finds an element for which the function returns false. This is exactly what we need to pack consecutive duplicate elements. The strategy is to pack all elements with the same value of the list head element, then store this list and recursively call `span()` on the remaining list.

``` scala
def pack[A](l: List[A]):List[List[A]] = {
    def _pack(res: List[List[A]], rem: List[A]):List[List[A]] = rem match {
        case Nil => res
        case ls => {
            val (s: List[A], r: List[A]) = rem span { _ == rem.head }
            _pack(res:::List(s), r)
        }
    }
    _pack(List(), l)
}
```

The function I pass to `span()` is simply `{ _ == rem.head }` since the method gives the function each element in the list.

## Final considerations

This problem helped me enhance my knowledge of **pattern guards** and Scala **anonymous functions**. The solution with `span()` includes a pattern match that leads to a **block expression**, which was somehow a syntactical blind guess for me, but ended up being perfectly valid.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.

