Title: 99 Scala Problems 08 - Eliminate consecutive duplicates of list elements
Date: 2015-04-07 09:35:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-08-eliminate-consecutive-duplicates
Series: "99 Scala Problems"
Summary: 

## The problem

**P08** Eliminate consecutive duplicates of list elements. If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example:

``` scala
scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
```

## Initial thoughts

This task seems to be easy to accomplish. It should be all about filtering incoming elements based on the last element added to the list.

## The recursive solution

``` scala
def compress[A](l: List[A]):List[A] = l match {
    case Nil => Nil
    case h::List() => List(h)
    case h::tail if (h == tail.head) => compress(tail)
    case h::tail => h::compress(tail)
}
```

Instead of adding one element and then discarding the successive if equal, the point here is to discard elements until a new one pops up.

## Folding

The folding methods of `List` types are very suitable for this task, as they scan the whole list carrying the result of previous applications.

I already used `foldLeft()` so this was my first pick

``` scala
def compress[A](l: List[A]):List[A] = l.foldLeft(List[A]()) {
    case (List(), e) => List(e)
    case (ls, e) if (ls.last == e) => ls
    case (ls, e) => ls:::List(e)
}
```

The initial value given to the folding method is an empty list. Then if the previous result is empty (i.e. when scanning the first element) the result is a list with that element. If the current element is already in the result (second case) the result is returned unchanged. Otherwise, the new element is appended at the end of the result.

Since appending a list to an empty list is just like returning the list itself we may join the first and third cases with a modified pattern guard

``` scala
def compress[A](l: List[A]):List[A] = l.foldLeft(List[A]()) {
    case (ls, e) if (ls.isEmpty || ls.last != e) => ls:::List(e)
    case (ls, e) => ls
}
```

Left folding implies appending new values at the end of the result list, which is syntactically less straightforward than appending at the beginning (`list:::List(t)` vs `h::list`). So we may fold from right with `foldRight()`

``` scala
def compress[A](l: List[A]):List[A] = l.foldRight(List[A]()) {
    case (e, ls) if (ls.isEmpty || ls.head != e) => e::ls
    case (e, ls) => ls
}
```

Remember that `foldLeft()` accepts a tuple in the form `(result, element)`, while `foldRight()` wants a tuple in the form `(element, result)`.

## Final considerations

Folding became more clear with this exercise. It's a good thing to learn ho to reverse algorithms to use `foldLeft()` or `foldRight()` according to the task we are performing.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.

