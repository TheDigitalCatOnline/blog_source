Title: 99 Scala Problems 14 - Duplicate the elements of a list
Date: 2015-04-14 12:00:00 +0100
Modified: 2019-02-28 10:00:00 +0000
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-14-duplicate-the-elements-of-a-list
Series: 99 Scala Problems
Image: 99-scala-problems
Summary: 

# The problem

**P14** (*) Duplicate the elements of a list.

Example:

``` scala
scala> duplicate(List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
```

# Initial thoughts

Once again (see [problem 12]({filename}99-scala-problems-12-decode-a-run-length-encoded-list.markdown)) a perfect task for `flatMap()` since for each element a list must be produced, but the resulting list shall be flat.

# Solution

The function passed to `flatMap()` this time shall return a list with two elements for each element of the source list.

``` scala
def duplicate[A](l:List[A]):List[A] = {
    l flatMap { e => List(e, e) }
}
```

# Final considerations

This problem didn't involve new concepts, but allowed me to test again **mapping** and **anonymous** functions.

# Feedback

The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.
