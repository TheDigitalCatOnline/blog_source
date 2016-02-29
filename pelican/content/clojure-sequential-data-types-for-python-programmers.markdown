Title: Clojure sequential data types for Python programmers
Date: 2015-09-22 19:00:00 +0100
Category: Programming
Tags: functional programming, Python, Python2, Python3, Clojure
Authors: Leonardo Giordani
Slug: clojure-sequential-data-types-for-python-programmers
Summary: A brief comparison of Clojure sequential data types (vectors and lists) with Python lists and tuples

I have been working with Python for more than fifteen years and I developed very big systems with this language, learning to know and love both its power and its weaknesses, admiring its gorgeous object-oriented system and exploring some of its darkest corners.

Despite being so committed to this language (both for personal choices and for business reasons) I never stopped learning new languages, trying to get a clear picture of new (or old) paradigms, to explore new solutions to old problems and in general to get my mind open.

Unfortunately introducing a new language in your business is always very risky, expensive and many times not a decision of yours only. You are writing software that other may have to maintain, so either you evangelize your team and persuade other to join you or you will be forced to stay with the languages shared by them. This is not bad in itself, but sometimes it is a big obstacle for the adoption of a new language.

One of the languages I fell in love (but never used in production) is LISP. I strongly believe that everyone calls themselves a programmer shall be somehow familiar with LISP, as well as with Assembly language. The reasons are many and probably do not fit well with the title of this article so I will save them for another, more theoretical post, and move on.

Clojure is a LISP implementation that runs on the JVM, thus being available on a wide number of platforms. As any other LISP its syntax is a step lower in the abstraction tree: while other languages provide their syntax and then a conversion to an AST (Abstract Syntax Tree), LISPs provide only their AST syntax. This has the great advantage of homoiconicity, something that they share with Assembly and other languages like Prolog, but comes at the cost of a slightly more difficult syntax, at least for the novice.

In this post I want to review the sequential data types provided by Clojure, trying to introduce them to programmers accustomed to Python data types. Anyway, while the nomenclature and the examples will be specific to Python, I think the discussion can be easily extended to other dynamic languages such as Ruby.

## Three collection types for the programming-kings

In Clojure, just like in Python, you find three types of basic collection types, that is types of data that hold together other types of data, also called compound data types. These are sequences, sets and maps.

Python provides two types of basic sequences, **tuples** and **lists**, while maps are called **dictionaries**. Those are present in Python since its first versions. **Sets** have been introduced with Python 2.3 in 2003, at first with a dedicated `set` module, which was then deprecated with Python 2.6 in favour of the built-in type with the same name. From Python 2.7 sets get also a dedicated short syntax similar to that of dictionaries (e.g. `{'just', 'a', 'set'`).

The two basic Python sequences differ mainly for a small but very important detail: mutability. In Python tuples are immutable, while lists can be modified. Python lists behave mostly like C arrays: elements can be appended to and extracted from the right end. Apart from the mutability, Python lists and tuples expose a common API which we may address as the **Python sequence API** or **protocol**.

An object that follows the sequence API in Python has the following traits, among the others:

* Elements are indexed starting from 0 up to the total number of elements minus 1.
* The sequence can be traversed in order, that is, elements can be visited one by one and each repeated visit of the whole collection presents the elements in the same order.

A derived feature which is provided by sequences is that they can be sliced, i.e. part of them can be extracted as a collection of the same type.

These features are paramount because they allow Python to define a common interface for loops. This is obviously shared with mostly every programming language, but Clojure implements it in a slightly different way. A Python class shall implement some "magic" methods to expose the sequence API. To get a starting picture of the steps involved read [this post](/blog/2015/05/13/python-oop-tdd-example-part1/), where I discuss the implementation of a binary number type in Python.

## A matter of definitions

The Clojure counterpart of the Python sequence protocol is the **seq API** and it consists of two simple functions: `first` and `rest`. Their behaviour is the following

* If the collection is not empty `(first collection)` returns the first element, otherwise returns `nil`.
* If the collection is not empty `(rest collection)` returns a new sequence containing all the elements of `collection` except the first one, in the same order. If the collection is empty `rest` returns an empty collection.

This API is the same we can find in functional languages like Erlang or Scala, where most of the loops are implemented as (tail-) recursive function calls that consume the sequence one element at a time, leaving the rest of the sequence to the next recursion.

Pay attention to an important difference in Clojure's handling of empty sequences, compared to other LISPs and Python itself. While those treat empty sequences as logically false, Clojure doesn't, so be careful when you deal with this part of it coming from a different language. Remember that in Clojure **everything is true except `nil` and `false`**.

Given the definition of the *seq* API, in Clojure we call **sequence** everything behaves according to it. Since Clojure is implemented on the JVM this "behaviour" matches a very specific Java interface, which in this case is `ISeq` (see the source definition [here](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/ISeq.java)).

The concept of being an ordered set of values in Clojure is represented by the word **sequential**, which corresponds to the Java interface `Sequential` ([source code](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Sequential.java)). Last, the concept of being a **collection** is represented in Clojure by means of the Java interface `IPersistenCollection` ([source code](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/IPersistentCollection.java)).

Given these definitions it is immediately clear what difference is there between the `seq?`, `sequential?` and `coll?` functions. They tell you if the given parameter implements the relative Java interface.

The function `seq`, instead, performs a conversion of its input into a sequence, that is something that implements the `ISeq` interface. This function always returns `nil` if the input is an empty collection, which allows Clojure to adhere to the standard LISP technique called *nil-punning*, or in simpler words **loop termination condition check**. Every collection in Clojure provides a meaningful output for the `seq` function, but some of them provide more than one function to return a sequence. For example maps provide a way to get the sequence of both keys and values through the aptly named `keys` and `vals` functions.

## Vectors and lists

Clojure provides two sequential data types just like Python, but they do not differ for the mutability property, because in Clojure every collection is immutable. Instead, they differ for their behaviour as queues (where a new element goes and where a popped element comes from).

These two data types are vectors and lists, and as happens in other LISPs lists are very important for the language syntax itself. Their main difference between the two types, which is immediately clear when using them, is that while vectors grow on the right end, lists grow on the left one. This will be important when discussing append functions like `conj`.

Let me first start with some details on mutability and persistence.

## Mutability and persistence

Clojure's collections are **immutable**, which means that you cannot change the content of one of their instances. This is a feature provided in Python by means of the tuple data type only. As happens in other languages like Erlang, Clojure's immutability of collections is a trampoline to concurrency safety, something the language provides as a feature rather than a limitation.

Clojure collections are also **persistent**. The original meaning of this word refers to the possibility of accessing the old version of a given structure after having changed it. Since structures in Clojure are immutable you are forced to create a new variable every time you want to change something, which implies that the "old version" is still available. That is, immutability implies persistence. In Clojure, moreover, structures share values as long as this is consistent, which is exactly what Python does with the references concept (see [this post](/blog/2014/08/21/python-3-oop-part-4-polymorphism)).

## Vectors in Clojure

Clojure's vectors are very similar to Python lists. The first thing that makes them similar (apart from the square brackets syntax) is that they grow on the right end.

``` clojure
user=> (conj [1 2 3 4] 5)
[1 2 3 4 5]
```

``` pycon
>>> l = [1,2,3,4]
>>> l.append(5)
>>> l
[1, 2, 3, 4, 5]
```

From the right end you can also easily get values

``` clojure
user=> (peek [1 2 3 4 5])
5
```

``` pycon
>>> l = [1,2,3,4,5]
>>> l
[1, 2, 3, 4, 5]
>>> l.pop()
5
>>> l
[1, 2, 3, 4]
```

So Clojure vectors and Python lists are LIFO queues (Last In First Out), otherwise known as *stacks*. In Clojure `peek` takes the role of the classic `pop` operation, but remember that being collections immutable, what you get is a new value. That is, while in Python `pop()` modifies the list, in Clojure `peek` doesn't alter the vector.

### Subvectors

Just like Python lists, vectors can be sliced with the `subvec` function

``` clojure
user=> (subvec [:a :b :c :d :e] 2 5)
[:c :d :e]
```

Subvectors are however different from Python list slices. When you slice a list in Python you get a new object containing part of the same references you can find in the source list. That is, slicing a list actually builds a new list copying the reference of each object contained in the slice.

Let us briefly review this important concept. For this example I build a list of mutable objects, in this case dictionaries.

```pycon
>>> l = [{'a': 1, 'b': 2}, {'c': 3, 'd': 4}, {'e': 5, 'f': 6}]
>>> l
[{'b': 2, 'a': 1}, {'c': 3, 'd': 4}, {'f': 6, 'e': 5}]
```

then take a slice of it, a list containing the second element only

``` pycon
>>> s = l[1:2]
>>> s
[{'c': 3, 'd': 4}]
```

The original list `l` and the slice `s` are different objects, as shown by their memory address

``` pycon
>>> id(l)
3069706412
>>> id(s)
3061925100
```

but there is still a link between them, as you can see if you change the value of the element contained in the slice. This change affects the original list too

``` pycon
>>> s[0]['c'] = 100
>>> s
[{'c': 100, 'd': 4}]
>>> l
[{'b': 2, 'a': 1}, {'c': 100, 'd': 4}, {'f': 6, 'e': 5}]
```

In Clojure a subvector is a complete reference to the source vector, with a special annotation of the slice boundaries. This gives you a very fast way to create subvectors, which is independent from the size of the vector itself (aka O(1) or constant time). On the other hand this gives you a structure which is a bit slower when accessed, due to the math involved in computing the actual offset. This handicap however does not propagate to subvectors of subvectors, so it can be considered a (little) constant slowdown of every subvector.

### Appending values (push)

You find two different functions that can append values to a Clojure vector, `into` and `conj`, implementing the `push` operation of classic stacks. Clojure is not an object-oriented language, so these functions are not methods of the vector type. Those methods however behave in a different way according to the destination type.

The `into` function, according to the [official documentation](https://clojuredocs.org/clojure.core/into) *conjoins every item* of the second argument (the *from* collection) into the first argument (the *to* collection). This means that every item of the incoming collection is appended to the destination, according to the way that collection manages the append operation. For example, vectors grow on the right, so performing an `into` will append elements on the right end of the vector itself. This means also that `into` may be used to convert one type of collection into another.

Conversely, the `conj` function *conjoins the whole second argument* (`conj` accepts more than two arguments but let's deal with the simpler case now)

``` clojure
user=> (conj [1 2 3 4] [5 6])
[1 2 3 4 [5 6]]
```

Just like `into`, `conj` relies on the destination type to perform the actual append operation.

In Python you can find a similar situation if you consider the two methods `extend()` and `append()` of lists. The first one expects an argument that exposes the sequential protocol, and pushes into the destination list each of its elements

``` python
>>> l = [1, 2, 3]
>>> l
[1, 2, 3]
>>> l.extend([4, 5, 6])
>>> l
[1, 2, 3, 4, 5, 6]
```

On the other hand, `append()` pushes into the list the whole argument, just like `conj` does for vectors in Clojure, and thus is mostly used to append single elements, for example in for loops.

``` python
>>> l = [1, 2, 3]
>>> l.append([4, 5, 6])
>>> l
[1, 2, 3, [4, 5, 6]]
```

### Getting values (pop)

The `pop` operation of classic stacks is also implemented by two different functions, `peek` and `pop`. The first one returns the last element of the vector, being thus the Clojure's equivalent of Python's `pop()` as shown in a previous example. Clojure's `pop`, instead, removes the last element and returns the resulting vector.

``` clojure
user=> (pop [1 2 3 4 5])
[1 2 3 4]
```

The Python equivalent could be a slice

``` pycon
>>> [1,2,3,4,5][:-1]
[1, 2, 3, 4]
```

The `last` function will return the same value as `peek`, but is very inefficient for vectors. The [official documentation](https://clojuredocs.org/clojure.core/peek) gives an example for a vector of ten thousand elements where `last` is around 25 times slower than `peek`. This is mainly due to the fact that `last` scans the whole sequence to find the last element, that is works in linear time O(n) (see the [source code](https://github.com/clojure/clojure/blob/clojure-1.7.0/src/clj/clojure/core.clj#L249)).

A final note about looking for values. The Python `in` operator has no direct equivalent for Clojure vectors. Beware that the `contains?` function does check the collection keys (indices of the vector) and not the values. The [official documentation](https://clojuredocs.org/clojure.core/contains_q) of `contains?` gives some hints about this problem. I strongly suggest you to check them.

### Random access

Clojure vectors and Python lists are both arrays of indexed values, which means that they are optimized for random access. In Python you may just index the list

``` python
>>> l = ["a", "list", "of", "strings"]
>>> l[2]
'of'
```

where the indexing syntax works also for slices. In Clojure you can use the `nth` function

``` clojure
user=> (nth ["a" "vector" "of" "strings"] 2)
"of"
```

For vectors the `get` function maps to `nth`, since vectors are after all ordered dictionaries with the position indices as keys. Vectors in Clojure can also be directly used as functions, which aliases the use of `nth`

``` clojure
user=> (["a" "vector" "of" "strings"] 2)
"of"
```

## Lists in Clojure

Being Clojure a LISP, lists are of paramount importance and a complete discussion about them will surely span more than a small post on a blog. So in this section I will try to stay focused on the differences with Python lists (and Clojure vectors), rather than give you a complete overview of this incredibly powerful tool. The most important thing I will not review here is that lists in Clojure are part of the syntax itself and in the following section I will only consider lists as data containers. The fact that LISPs consider code as data is outside the scope of this work.

Being vectors the Clojure collection that implement classic stacks (LIFO queues), and being stacks one of the most used data structures in computer science, it is no surprise that a lot of sequential data processing in Clojure is done through vectors.

As already stated the big difference between vectors and lists in Clojure is the fact that lists grow on the left end. Let us review the behaviour of `peek` and `conj` on lists compared to that of the same functions on vectors

``` clojure
user=> (peek '(1 2 3 4))
1
user=> (peek [1 2 3 4])
4
user=> (conj '(2 3 4) 1)
(1 2 3 4)
user=> (conj [2 3 4] 1)
[2 3 4 1]
user=> (pop '(2 3 4))
(3 4)
user=> (pop [2 3 4])
[2 3]
```

I think this is simple to remember and easy to deal with in algorithms. As you can see lists behave somehow like mirrored vectors. Beware, however, that functions such as `first` and `last` still work in the same way. That is, lists are not reversed vectors.

As happened for vectors there are some performance considerations to do for lists. Even for lists the `last` function performs in linear time, but while vectors can use `peek` to perform the same service, lists cannot. This is a big asymmetry and one of the reasons you could prefer vectors over lists for your data. While accessing the first element is blazing fast both for lists and vectors, accessing the last can be very fast for vectors through the `peek` function but is performed in linear time on lists.

One of the biggest drawbacks of Clojure lists used like arrays is that they are not optimized for random access, while vectors are. That is, the `nth` function performs in linear time on lists since, just like `last`, it scans the whole list one element at a time. Vectors, on the other hand, are indexed, which means that random access is performed in constant time.

As you can see there is not much to say about Clojure lists as data containers (here *data* is used more in a Python sense): there are simply very few reasons to prefer lists over vectors as data containers.

## Final words

Python and Clojure are very different languages, so this was not an attempt to find "migration rules" from Python code to Clojure. I just wanted to review what Python programmers shall expect from Clojure sequential data types, expressing it in a familiar way.

I wish you a happy Clojure time!

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
