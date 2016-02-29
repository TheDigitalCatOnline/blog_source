Title: 99 Scala Problems 05 - Reverse a list
Date: 2015-04-07 09:20:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-05-reverse
Series: "99 Scala Problems"
Summary: 

## The problem

**P05** (*) Reverse a list.

Example:

``` scala
scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1, 1)
```

## Initial thoughts

Another classical problem of computer science and functional languages. The solution will be straightforward. As happened with the `length()` function developed for [problem 04](/blog/2015/04/07/99-scala-problems-04-length/), this one may help solving [problem 02](/blog/2015/04/07/99-scala-problems-02-find-last-nth/) in a pure recursive way. Moreover this problem may be solved with folding like problem 04.

## The procedural solution

`List` type provides a suitable built-in method `reverse()` to return a reversed copy of the list. Pay attention to the fact that this method does not reverse the list in place, but returns a new list.

``` scala
def reverse[A](ls: List[A]) = ls.reverse
```

## The recursive solution

``` scala
def reverse[A](l: List[A]): List[A] = l match {
    case h :: tail => reverse(tail) ::: List(h)
    case Nil => Nil
}
```

This solution gives us the chance to dig into list concatenation operators in Scala. The double colon operator `::` has been introduced in problem 01 as something that divides the head element of a list from the tail. As a matter of fact, that operator appends an element at the beginning of a list

``` scala
scala> val l = List(2,3,4,5,6)
l: List[Int] = List(2, 3, 4, 5, 6)

scala> 1::l
res0: List[Int] = List(1, 2, 3, 4, 5, 6)
```

Pay attention that pattern matching does not only a "pattern comparison", but also an assignment. In Scala, when you write

``` scala
scala> 5 match {
     |  case a => a + 1
     | }
res3: Int = 6
```

what you do is to first try to match `5` with `a`, and if it is successful, Scala assigns the value `5` to the variable (in this case `a` is a free variable, so the match is always satisfied).

So the list pattern matching we already used

``` scala
scala> List(1,2,3,4) match {
     |  case h::tail => h
     |  case _ => Nil
     | }
res7: Any = 1
```

tries to match the incoming `List(1,2,3,4)` with something in the form `h::tail`, which means "a list with a single element head and a list tail". Using the `::` operator ensures that `h` is considered a single element.

This, however, is just a mnemonic. The truth is that `::` in Scala is also [a class](http://www.scala-lang.org/api/2.11.4/index.html#scala.collection.immutable.$colon$colon) derived from `List` which is constructed passing a single element and a list. This means that the above example may be explicityl written as

``` scala
scala> List(1,2,3,4) match {
     |  case ::(h,tail) => h
     |  case _ => Nil
     | }
res7: Any = 1
```

So, while the effect is the same, there is difference between `::` as a method of `List` objects and `::` as a class used in pattern matching. Also remember that in Scala methods which name ends with `:` are right-associative (see [the language specification](http://www.scala-lang.org/files/archive/spec/2.11/06-expressions.html#infix-operations). This means that `0 :: List(1,2,3)` is a shorthand form for `List(1,2,3).::(0)`

The triple colon operator `:::`, conversely, is just a method of `List` objects and there is no class with that name. It prepends a whole list to another list (joins the lists), and ending with `:` it is also a right-associative method.

``` scala
scala> List(1,2,3) ::: List(4,5,6)
res18: List[Int] = List(1, 2, 3, 4, 5, 6)

scala> List(4,5,6).:::(List(1,2,3))
res0: List[Int] = List(1, 2, 3, 4, 5, 6)
```

Now the recursive solution becomes more clear: the first pattern matching case just exchanges the head of the list with the tail, but recursively calls the function on the tail itself.

As already explained this code is not tail recursive, thus allocating stack space to store local variables during the recursive call. A good way to make it tail recursive is to use another list that is filled by the elements extracted from the source one.

``` scala
def reverse[A](l: List[A]): List[A] = {
    def _reverse(res: List[A], rem: List[A]): List[A] = rem match {
        case Nil => res
        case h :: tail => _reverse(h :: res, tail)
    }
    _reverse(Nil, l)
}
```

Here we use an helper function in which we pick the first element of the remainder list `rem` and add it at the beginning of a list built from scratch. As you can see there is no computation left on the stack when we do the recursive call. The first solution, conversely, gave the result of the recursion as input to the `:::()` method.

## Folding

Folding may greatly simplify the solution. Remember that `foldLeft()` is a method of `List` objects that visits each element in the list, applying a given function. The first use of the function receives the initial value passed to `foldLeft()`, while successive calls receive the result of the previous call.

``` scala
def reverse[A](ls: List[A]): List[A] =
    ls.foldLeft(List[A]()) { (r, h) => h :: r }
```

The function passed to `foldLeft()` accepts the previous result and the current element. Since the initial value is an empty list we may apply the `::` operator just like we did in the tail recursive solution.

## Last nth element

``` scala
def lastNth[A](n: Int, l:List[A]): A = {
    def reverse[A](l:List[A]):List[A] = {
        def _reverse[A](r:List[A], l:List[A]):List[A] = l match {
            case Nil => r
            case head::tail => _reverse(head::r, tail)
        }
        _reverse(List(), l)
    }
    
    def findKth[A](k:Int, l:List[A]):A = (k,l) match {
        case (0, h::_) => h
        case (k, _::tail) if k > 0 => findKth(k - 1, tail)
        case _ => throw new NoSuchElementException
    }

    val r = reverse(l)
    findKth(n - 1, r)
}
```

Instead of computing the actual index of the last-nth element using the `length()` function, we may just reverse the list and get the (n - 1)-th element (the subtraction of 1 comes from the fact that last-nth elements are indexed from 1, while list indexes start from 0).

## Final considerations

This problem led me into the **list concatenation operators** and the discovery of the `::` class that allows **list decomposition** in pattern matching.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
