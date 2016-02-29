Title: 99 Scala Problems 02 - Find the last but one element of a list
Date: 2015-04-07 09:05:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-02-find-last-nth
Series: "99 Scala Problems"
Summary: 

## The problem

**P02** (*) Find the last but one element of a list.

Example:

``` scala
scala> penultimate(List(1, 1, 2, 3, 5, 8))
res0: Int = 5
```

## Initial thoughts

This problem come in the same form as the first one, so most of the syntax issues have already been solved. The challenge here is to find a good algorithm to extract the penultimate element and, as done for the first problem, it can probably be solved both in a procedural and in a recursive way.

Theoretically, the issue is very simple: we have to cycle (or recursively call the function) as long as we find the last element. Then, the penultimate is the last element read.

As happened for the first problem, however, both the procedural and the functional solution provide some smart way to implement that algorithm.

An additional interesting issue is to try and generalize the problem, writing a function that extracts the last-nth element, where the last-1st is the last element, the last-2nd is the penultimate, and so on.

There are two edge cases this time, but both give the same result. When the list is empty or the list contains only one element, the penultimate element cannot be retrieved. In general, if we talk about the last-nth algorithm, the list shall contain at least n elements.

The general function to extract the last-nth element shall also check that no negative indexes are provided.

## The procedural solution

A smart way to solve the problem is to consider that the penultimate element of a list of n elements is the last element of the first (n - 1) elements. Scala provides a method called [`init()`](http://www.scala-lang.org/api/2.11.4/index.html#scala.collection.immutable.List) which returns all elements except the last. Documentation says that if the given list is empty the method throws the `UnsupportedOperationException`, so if we want to imitate the previous `last()` function we shall cover the empty list case.

If the list contains a single element, `init()` returns an empty list, and in that case the `last()` method throws the right exception.

``` scala
def penultimate[A](l:List[A]):A = {
    if (l.isEmpty) throw new NoSuchElementException  
    l.init.last
}
```

The generic case may be handled using the `takeRight()` method, which selects the last n elements from the list. The last-nth element is then the head of the resulting list. This method deals with a list with less than n elements returning the whole list, which is not what we want. So we have to deal with the case of a list which contains less than n elements. The length of a list in Scala may be retrieved with the `length()` method.

``` scala
def lastNth[A](n: Int, l: List[A]): A = {
    if (n <= 0) throw new IllegalArgumentException
    if (n > l.length) throw new NoSuchElementException
    l.takeRight(n).head
}
```

## The recursive solution

The recursive solution takes advantage of the power of pattern matching. The exit case is when the list is composed by a head element and a tail made by a list of one single element. The pattern matching syntax can express this situation with the `h :: List(t)` syntax. The standard case is when the tail is still a generic list and the last case covers all edge cases.

``` scala
def penultimate[A](l:List[A]):A = l match {
    case h :: List(t) => h

    case _ :: tail => penultimate(tail)

    case _ => throw new NoSuchElementException
}
```

The generic case may be written at least in two different ways. The first one involves the use of an helper function that reverses the list. Once the list has been reversed we may recursively find the nth element. Another way to express the same procedure is to have an helper function that counts the number N of elements in the list, then finds the (N-n)-th element (zero-indexed). Since however the reverse function, the count function, and the function to extract the nth element are discussed in the next three problems (problem 03, 04 and 05) this solution will be shown there.

The second solution makes use of some methods of the `List` type and pattern guards. Pattern guards are simply conditional expressions that rule the application of a pattern.

``` scala
def lastNth[A](n: Int, l:List[A]): A = l match {
    case tail if (tail.length == n) => tail.head
    case _ :: tail => lastNth(n, tail)
    case _ => throw new NoSuchElementException
}
```

The function is very simple. If the length of the list is `n` the element we are looking for is the first (here the pattern guard does the job). Otherwise if the list has still some tail just call the function recursively on it and last, that is if the list is empty, throw the exception.

## Final considerations

This time I learned digging into the **Scala documentation** and paying attention to **exceptions** which are an important matter in a robust API.

Working on the recursive solution I learned that **patterns** can be very expressive and how to use **pattern guards**.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
