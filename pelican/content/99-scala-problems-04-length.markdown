Title: 99 Scala Problems 04 - Find the number of elements of a list
Date: 2015-04-07 09:15:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-04-length
Series: "99 Scala Problems"
Summary: 

## The problem

**P04** (*) Find the number of elements of a list.

Example:

``` scala
scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
```

## Initial thoughts
This should be straightforward both with object-oriented and functional techniques. There are no special cases except that of an empty list. There is an interesting solution that mixes functional and object-oriented techniques, which is known as folding, and I'll discuss it in detail.

As already said for [problem 02](/blog/2015/04/07/99-scala-problems-02-find-last-nth/), this function may help solving that problem in a pure recursive way, so we'll talk about this too.

## The procedural solution

``` scala
def length[A](l:List[A]):Int = {
    l.length
}
```

No special things to high light here. The `length()` method works with empty lists too, returning 0, so everything should be fine. As already shown in [problem 01](/blog/2015/04/07/99-scala-problems-01-find-last-element/) the function may be also reduced to one single line

``` scala
def length[A](l:List[A]):Int = l.length
```

## The recursive solution

Coming from procedural languages my first solution was

``` scala
def length[A](l:List[A]):Int = {
    def lengthN[A](n:Int, l:List[A]):Int = l match {
        case Nil => n
        case _::tail => lengthN(n + 1, tail)
    }
    lengthN(0,l)
}
```

which works fine. I create an helper function that for each element in the list calls itself passing an incremented accumulator and the remaining elements of the list. Then I call it passing an empty accumulator and the whole list.

There is a simpler way to express the same algorithm, which is

``` scala
def lengthRecursive[A](l:List[A]):Int = l match {
    case Nil => 0
    case h::tail => 1 + lengthRecursive(tail)
}
```

The first solution is better, anyhow, because that function is tail recursive. You can find a good explanation of tail recursion [here](http://oldfashionedsoftware.com/2008/09/27/tail-recursion-basics-in-scala/). The explanation given by "learn you some Erlang!" ([here](http://learnyousomeerlang.com/recursion)) is perhaps clearer, even if discussed for a different language.

Basically the first solution may be optimized since all local variables are useless once the recursive call is performed, thus the stack may be freed.

## Folding

There is a third way to solve the problem, mixing object-oriented and functional approaches, through the use of folding. This is something new for me, and it took me a little to grasp the concept.

``` scala
def length[A](l:List[A]):Int = l.foldLeft(0) { (c,_) => c + 1 }
```

The `foldLeft()` definition is the following

``` scala
def foldLeft[B](z: B)(f: (B, A) ⇒ B): B 
```

and the documentation states: "Applies a binary operator to a start value and all elements of this sequence, going left to right".

So basically the `foldLeft()` method visits each element of the list from left to right and applies a function passing the result of the previous application. The first element receives the initial value.

The first strange thing that I see here is that the function is defined with two sets of brackets, the first encompassing "the start value" `z` and the second what seems the definition of a function.

This has to do with the concepts of partially applied functions (NOT *partial functions*) and currying. I recommend two readings from Stackoverflow: [this answer](http://stackoverflow.com/questions/8650549/using-partial-functions-in-scala-how-does-it-work/8650639#8650639) about the meaning of partial function, partially applied function and currying, and [this answer](http://stackoverflow.com/questions/14309501/scala-currying-vs-partially-applied-functions) (which also links the first one) that explores the difference between partially applied and currying. I consider that two answers to be exhaustive so I will not add any explanation about this topic.

There is another thing to note in the folding solution, however, which is the way Scala defines anonymous functions. The Scala tour has a [good explanation](http://docs.scala-lang.org/tutorials/tour/anonymous-function-syntax.html) of anonymous functions, which are pretty simple.

In the given solution for the problem, the anonymous function can be written more explicitly like

``` scala
def count[A](c:Int, d:A):Int = {
    c + 1
}

def length[A](l:List[A]):Int = l.foldLeft(0) { count }
```

which should be clear. The count function accepts an `Int` and a value of the generic type `A` and returns `c + 1`. This function is applied on each element of the list by the `foldLeft()` method, passing the result of the previous application as the first parameter `c`.

The `count()` function may however be simplified to an anonymous function due to its simplicity. Making use of [type inference](http://stackoverflow.com/questions/4899320/when-does-scala-need-parameter-types-for-anonymous-and-expanded-functions) and [placeholder syntax](http://www.codecommit.com/blog/scala/quick-explanation-of-scalas-syntax) we may reduce it to the given form.

## Last nth element

Now that we have a pure recursive solution for counting elements in a list we may go back to [problem 02](/blog/2015/04/07/99-scala-problems-02-find-last-nth/) and give another solution.

``` scala
def lastNth[A](n: Int, l:List[A]): A = {
    def length[A](l:List[A]):Int = {
        def lengthN[A](n:Int, l:List[A]):Int = l match {
            case Nil => n
            case _::tail => lengthN(n + 1, tail)
        }
        lengthN(0,l)
    }
    
    def findKth[A](k:Int, l:List[A]):A = (k,l) match {
        case (0, h::_) => h
        case (k, _::tail) if k > 0 => findKth(k - 1, tail)
        case _ => throw new NoSuchElementException
    }
    val k = length(l) - n
    findKth(k, l)
}
```

This simply takes advantage of the function `length()` developed in this post and of the function `findKth()` from [problem 03](/blog/2015/04/07/99-scala-problems-03-find-kth/).

## Final considerations

The discussion about this problem involved a lot of important topics for the Scala programmer: **tail recursion**, **folding**, **partial functions**, **partially applied functions**, **currying**, **anonymous functions**, **type inference** and **placeholder syntax**. We also reached the goal of writing a pure recursive solution to problem 02.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
