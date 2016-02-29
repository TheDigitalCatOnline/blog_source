Title: 99 Scala Problems 03 - Find the Kth element of a list
Date: 2015-04-07 09:10:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-03-find-kth
Series: "99 Scala Problems"
Summary: 

## The problem

**P03** (*) Find the Kth element of a list.
By convention, the first element in the list is element 0.

Example:

``` scala
scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2
```

## Initial thoughts

I expect the solution to be trivial using the indexing capabilities of `List` types, such as in any language that supports indexed lists. The functional solution cannot use indexing and shall reduce the list element by element.

## The procedural solution

``` scala
def findKth[A](k:Int, l:List[A]):A = {
    if (k >= 0 && k < l.length) l(k)
    else throw new NoSuchElementException
}
```

Since indexing out of bounds (in both directions) throws `IndexOutOfBoundsException` we first check if the index is safe. The logical AND operation is expressed in Scala by `&&`.

Knowing that a wrong indexing throws an exception we could also catch it and throw the 'right' one. This results in 

``` scala
def findKth[A](k:Int, l:List[A]):A = {
    try l(k)
    catch {
        case e:IndexOutOfBoundsException => throw new NoSuchElementException
    }
}
```

which probably states more clearly that the main job of the function is to replace one exception with another one.

## The recursive solution

A recursive solution is pretty straightforward. We just repeatedly remove the first element until and decrease the given index until it reaches 0. Using a couple of methods from the `List` type the solution is

``` scala
def findKth[A](k:Int, l:List[A]):A = k match {
    case 0 => l.head
    case k if k > 0 => findKth(k - 1, l.tail)
    case _ => throw new NoSuchElementException  
}
```

If however we try to avoid the use of methods, just to see if we may solve it with a pure recursive approach, we need to match two things together, which leads to

``` scala
def findKth[A](k:Int, l:List[A]):A = (k,l) match {
    case (0, h::_) => h
    case (k, _::tail) if k > 0 => findKth(k - 1, tail)
    case _ => throw new NoSuchElementException
}
```

The "new" thing here is that we are pattern matching a tuple of values (which is, after all, one value of a given type). Moreover, Scala pattern matching shows us another strength point in that it can match a case and simultaneously operate on the variables (in this case splitting the list).

The first case is the exit case, where the counter reached 0 and the list has an head to return, which is the nth element. Otherwise the first element of the list is removed and the function called with a decremented index. The last line covers the empty list case.

## Final considerations

The procedural approach made me learn how to **catch exceptions** in Scala and how to perform **logical comparisons**. The recursive solution taught me **pattern matching of multiple values**.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
