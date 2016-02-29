Title: 99 Scala Problems 15 - Duplicate the elements of a list a given number of times
Date: 2015-04-14 12:30:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-15-duplicate-the-elements-of-a-list-a-given-number-of-times
Series: "99 Scala Problems"
Summary: 

## The problem

**P15** (**) Duplicate the elements of a list a given number of times.

Example:

``` scala
scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
```

## Initial thoughts

Problem 12 already taught me to use `fill()` to build lists of repeated elements so this could be a good occasion to use it again.

## Solution

This problem is a generalization of the previous [problem 14](/blog/2015/04/14/99-scala-problems-14-duplicate-the-elements-of-a-list) which required to duplicate the elements of a list. There we could build the list into an anonymous function with the `List(e, e)` syntax.

For this problem we have to use a more general solution, which is the `fill()` method of the `List` object (not class), already discovered in [problem 12]. This method is expressed in a curried form, thus the two argument applications.

Again, since the result of `fill()` is a list, we have to flatten it through `flatMap()`.

``` scala
def duplicateN[A](n: Int, l: List[A]):List[A] = {
    l flatMap { e => List.fill(n)(e) }
}
```

Scala allows us to express the anonymous function in a shorter way, making use of the underscore wild card.

``` scala
def duplicateN2[A](n: Int, l: List[A]):List[A] = {
    l flatMap { List.fill(n)(_) }
}
```

## Final considerations

Previous problems gave me all I needed to solve this one. I used **anonymous functions** and **mapping**, which are a very important component of Scala. I learned how to **simplify an anonymous function** using the underscore.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
