Title: 99 Scala Problems 01 - Find the last element of a list
Date: 2015-04-07 09:00:00 +0100
Category: Programming
Tags: Scala, functional programming
Authors: Leonardo Giordani
Slug: 99-scala-problems-01-find-last-element
Series: "99 Scala Problems"
Summary: 

## The problem

**P01** (*) Find the last element of a list.

Example:

``` scala
scala> last(List(1, 1, 2, 3, 5, 8))
res0: Int = 8
```

## Initial thoughts

This is the very first Scala problem, but a lot of topics are covered even by such a small task.

First I have to learn how to define functions. Then the problem shows two data types, namely `List` and `Int`, but it does not state that the list has to contain integers. So another point is to understand if and how Scala can deal with generic containers (that is, a list, regardless of the type of the content).

Scala has a strong functional component, so it seems a good thing to try and solve the problem both with some procedural code (object-oriented or not) and with a functional approach.

Well, the agenda is:

1. Get some knowledge about functions syntax
2. Learn generic containers
3. Find a procedural and a functional solution to the problem

## Functions

A function in Scala is defined through the `def` keyword, both inside and outside an object. The language syntax seems to have a lot of possibilities and some dark corners, but it is clear that input parameters have to be specified between round brackets, and the type of the output comes after them (outside the brackets). Then an equal sign and the body of the function between braces.

``` scala
def name_of_the_function(paramA:typeA, paramB:typeB, ...):type = {
    body
}
```

The equal sign has an important meaning, if I remember correctly. Omitting the equal sign makes the function automatically return a `Unit` type (that is, no output), so the point seems to be: ask yourself if your function shall return something or not. The `Unit` return type may be also explicitly stated, in which case you have to add the equal sign. And interesting clarification (with two different points of view) may be found [here](http://stackoverflow.com/questions/944111/when-to-use-the-equals-sign-in-a-scala-method-declaration). As a beginner, I prefer to be explicit.

## Type variables

I need to declare a function that accepts a list of any kind of things. This problem falls under the term _polymorphism_, and in Scala can be achieved with _type variables_. A very quick introduction to the matter may be found [here](https://twitter.github.io/scala_school/type-basics.html). Basically for my problem I simply have to use the following syntax

``` scala
def func[A](l: List[A]):A = {
    ...
}
```

where the starting `[A]` signals that the function is a sort of template with a free type `A`, unknown at the moment. The same type is used later to say that the parameter `l` is a `List` of things, and `A` is the type of each of them. Last, this function returns one value of the `A` type, since the problems requires to find one specific element of the list.

## Find the last element of a list

The procedural approach to this problem is to simply iterate through the list storing the last visited element, until the end of the function is reached. At that point the last visited element is also the last element of the list.

A good list type, however, should also have methods to easily retrieve special elements, such as the first or the last element. According to the [Scala documentation](http://www.scala-lang.org/api/2.11.4/index.html#scala.collection.immutable.List) the `List` type provides a `last()` method which returns ("selects") the last element.

The functional approach is easy in this case. Usually functional solutions are built starting with the exit case, which for this problem is: the list contains only one element, which is the last one. This excludes the possibility for the list to be empty, so we should manage that event too. If the list has more that one element, we simply remove the first one and call the function recursively. Just like Erlang, Scala has a very powerful pattern matching mechanism, that may be leveraged to simplify expressing the algorithm.

The only edge case of this algorithm is when the list is empty. In that situation no element can be found, and an exception shall be thrown (Scala nomenclature).

## The procedural solution

``` scala
def last[A](l:List[A]):A = {
    l.last
}
```

Since the body of the function is so small I can also omit the braces and write

``` scala
def last[A](l:List[A]):A = l.last
```

There is no check for an empty list because the `last()` method already throws the `NoSuchElementException` when no element is present.

There is no need to use an explicit `return` statement, as functions always return the last expression.

## The recursive solution

``` scala
def last[A](l:List[A]):A = l match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
}
```

The value returned by the function is `l match {}`, that the execution of the pattern matching block on the input variable `l`.

The first pattern divides the list in two parts: one head element (`h`) and a tail formed by nothing. This is the case in which the input list has only one element.

The second matching takes the tail of the list (the first element is not even stored, it is ignored using the `_` wildcard) and calls itself on the remaining values.

The last matching is selected when the first two fail, that is when the list is empty. In that case an exception shall be thrown to match the behaviour of the `last()` method. This is not mandatory, but is probably something that the programmer is accustomed to, so provided that we document it, it is a good convention to adhere to.

In Scala the exception shall be created at the moment, just like a standard object, so the `new` keyword is needed before the name of the exception itself.

## Final considerations

My first experience with functional languages was Erlang, so I quickly learned to love **pattern matching** and the beauty of **recursive solutions**. The Scala pattern matching syntax was for me really simple to grasp. The procedural solution using the dedicated method was straightforward.

The **type variables** syntax is very useful. I come from Python, where the polymorphic approach is pushed to its limit, and all variables are references, thus implementing type variables from the very beginning. In Scala, types are explicitly declared, so the type variables syntax allows me to use collections without binding the code to a specific type.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
