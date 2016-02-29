Title: 99 Scala Problems 11 - Modified run-length encoding
Date: 2015-04-14 10:30:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-11-modified-run-length-encoding
Series: "99 Scala Problems"
Summary: 

## The problem

**P11** (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.

Example:

``` scala
scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
```

## Initial thoughts

The solution shall be a modification of that of [problem 10](/blog/2015/04/14/99-scala-problems-10-run-length-encoding-of-a-list), and the only difference is that this time we do not produce the same type of result for each element.

## Choices

If the source element is a list with more than an element, we produce a tuple `(Int, A)`, where `A` is the actual type of elements in the list. If the source element is a list with a single element, we just produce that element, so the type is `A`.

In Scala this situation may be represented by means of `Either`, `Left` and `Right`. Good explanations of this matter may be found [here](http://alvinalexander.com/scala/scala-either-left-right-example-option-some-none-null) and [here](http://danielwestheide.com/blog/2013/01/02/the-neophytes-guide-to-scala-part-7-the-either-type.html).

Given that, the solution is straightforward

``` scala
def encode[A](l: List[A]):List[Either[A, (Int, A)]] = {
    utils.packer.pack(l) map {
        e => e match {
            case e if e.length == 1 => Left(e.head)
            case e => Right((e.length, e.head))
        }
    }
}
```

I prefer to express conditions through pattern matching, but the same code may be expressed with an `if/else` statement.

## Final considerations

This time I learned how to use the three types **Either**, **Left** and **Right**. Reading the suggested posts I also dug into **Option**, **Some** and **None**, and the three similar types introduced with Scala 2.10 **Try**, **Success** and **Failure**.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
