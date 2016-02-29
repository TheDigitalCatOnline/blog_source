Title: 99 Scala Problems 07 - Flatten a nested list structure
Date: 2015-04-07 09:30:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-07-flatten
Series: "99 Scala Problems"
Summary: 

## The problem

**P07** (**) Flatten a nested list structure.

Example:

``` scala
scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)
```

## Initial thoughts

Flattening lists is a perfect application for recursive functions, and the algorithm shouldn't be too complex. The key point in flattening is the possibility to tell apart a list from a non-list element, to rule the call of another recursion. To solve this I will probably have to deal with typed patterns.

## The recursive solution

The `flatten()` method of `List` objects works only if the list contains "traversable collections".

``` scala
scala> List(List(1,2), List(3,4)).flatten
res2: List[Int] = List(1, 2, 3, 4)
```

But if we add a non-traversable element such as a single `Int` the list becomes a list of `Any` and there is no way for Scala to use them as traversable collections

``` scala
scala> List(List(1,2), List(3,4), 5).flatten
<console>:8: error: No implicit view available from Any => scala.collection.TraversableOnce[B].
              List(List(1,2), List(3,4), 5).flatten
                                            ^
```

So this problem cannot be solved with the simple application of a method. My first solution was this

``` scala
def flatten[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case (h:List[A])::tail => flatten(h):::flatten(tail)
    case (h:A)::tail => h::flatten(tail)
}
```

I have been very creative in trying to match a list or an element, but fortunately Scala seems to have a coherent syntax.

This code gives nevertheless problems when compiled. The following warning was printed by the compiler "_warning: there were 2 unchecked warnings; re-run with -unchecked for details_" and running it with the suggested options results in "_warning: non variable type-argument A in type pattern List[A] is unchecked since it is eliminated by erasure_". Another warning was issues for the `(h:A)` code in the third `case` statement.

Well [this page](http://www.artima.com/pins1ed/case-classes-and-pattern-matching.html) talks a lot about _typed patterns_ and _type erasure_ in Scala. The short story is that Scala compiles for the JVM, which does not keep type information for collections (this is a complex matter, and not knowing Java I don't fully grasp the whole thing now).

Another interesting resource about type erasure is [this Scalafied post](http://www.scalafied.com/60/lightweight-type-erasure-matching).

This version seems to be correct

``` scala
def flatten(l: List[Any]): List[Any] = l match {
    case Nil => Nil
    case (h:List[_])::tail => flatten(h):::flatten(tail)
    case h::tail => h::flatten(tail)
}
```

I had to change the input arguments to `List[Any]` and discard the type information. This function works as expected, but there is no check of the incoming data types.

The algorithm here splits the list in two, head and tail and joins them together again after they have been processed by `flatten()` itself. Its tail recursive version is rather simple

``` scala
def flatten(l: List[Any]): List[Any] = {
    def _flatten(res: List[Any], rem: List[Any]):List[Any] = rem match {
        case Nil => res
        case (h:List[_])::Nil => _flatten(res, h)
        case (h:List[_])::tail => _flatten(res:::h, tail)
        case h::tail => _flatten(res:::List(h), tail)
    }
    _flatten(List(), l)
}
```

## Flatmap

`List` objects provide a very interesting method, `flatMap()` that, just like `map()`, applies a given function to all elements of the list. While `map()` builds the resulting collection concatenating the results of each application, `flatMap()` concatenates the elements of the collection that results from each application.

The difference becomes evident with this simple example

``` scala
scala> List(1,2,3,4).map( e => List(e,e*2) )
res5: List[List[Int]] = List(List(1, 2), List(2, 4), List(3, 6), List(4, 8))

scala> List(1,2,3,4).flatMap( e => List(e,e*2))
res6: List[Int] = List(1, 2, 2, 4, 3, 6, 4, 8)
```

Here, `map()` is used to produce a list for each element containing the element itself and the element multiplied by two. As you can see the result of the `map()` method is a list of lists. `flatMap()`, conversely, returns the concatenation of all elements.

The point here is that the function given to `flatMap()` shall return a list, which is then flattened, by the method itself, in that its elements are taken from the list and directly put into the source list.

Our function could be expressed by the following sentence: if the element is a list, call the function recursively, otherwise return a list containing that element. How can we express such a function? Scala allows to define partial functions as case sequences (see [here](http://www.artima.com/pins1ed/case-classes-and-pattern-matching.html)) so the solution is pretty simple

``` scala
def flatten(l: List[Any]): List[Any] = l flatMap {
    case ls: List[_] => flatten(ls)
    case h => List(h)
}
```

Pay attention to the fact that this function has to drop the type check just like the first one.

## Final considerations

**Type erasure** is a new concept, and one shall be aware of it. **Partial functions as case sequences** are really handy, and so are the `map()` and `flatMap()` methods.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.

