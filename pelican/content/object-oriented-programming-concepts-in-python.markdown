Title: Object-Oriented Programming (OOP) concepts in Python
Date: 2020-04-26 00:00:00 +0100
Category: Programming
Tags: metaclasses, metaprogramming, OOP, Python, Python3
Authors: Leonardo Giordani
Slug: object-oriented-programming-concepts-in-python
Image: object-oriented-programming-concepts-in-python
Summary: A review of the main concepts that a beginner Python programmer should learn about OOP in Python

In this article I will review the main concepts a beginner Python programmer should learn about OOP in Python. The content is explored in detail in other posts on the blog that are linked at the end of each section.

## Fundamentals

Object-oriented programming (OOP) is a paradigm, that is _a way to structure your code_ that aims to make it _easy to maintain and modify_. Often OOP is considered opposed to functional programming (FP), but the truth is that both paradigms are useful and some modern languages implement both (Scala, for example). Python implements some features of the FP paradigm, but if you want to be a good Python programmer you have to understand OOP.

Moreover, since OOP is a conceptual paradigm, you have to learn how Python _implements_ OOP, that is, the specific rules of the Python universe, which are not true in other programming languages.

The core concept you have to learn is that a paradigm like OOP has a single goal: that of making code _reusable_ and _easy to change_. Keep it in mind, otherwise you will blindly learn rules without understanding, which means that you will not understand when you have to be strict and when you should bend them.

## Three pillars

OOP has three main pillars:

* **Data encapsulation** has been introduced to link data structures and functions working on them. Objects are created from _classes_, which promote the separation between private code (code used to manage the low-level details of the data structures) and public code (code available to other parts of the system).
* **Delegation** is the way OOP promotes _code reuse_. Through delegation, objects in a system can collaborate and avoid duplicating code. It can be implemented through _inheritance_ (implicit delegation) or _composition_ (explicit delegation).
* **Polymorphism** is how OOP promotes _algorithm reuse_. When polymorphism is correctly implemented, objects with completely different internal structures can be processed by the same algorithm and collaborate. This promotes _refactoring_ and _code maintenance_ allowing to replace old code with new implementations.

## Resources

To learn how Python implements these three main concepts you can read the following posts:

* Data encapsulation: [Object-Oriented Programming in Python 3 - Objects and types]({filename}python-3-oop-part-1-objects-and-types.markdown) and [
Object-Oriented Programming in Python 3 - Classes and members]({filename}python-3-oop-part-2-classes-and-members.markdown)
* Delegation: [Object-Oriented Programming in Python 3 - Composition and inheritance]({filename}python-3-oop-part-3-delegation-composition-and-inheritance.markdown)
* Polymorphism: [Object-Oriented Programming in Python 3 - Polymorphism]({filename}python-3-oop-part-4-polymorphism.markdown)

These are all part of the same series of posts, so you can read them in order to have a complete picture. The same concepts are also explained in the following videos that I published on YouTube:

* Video [Object-oriented programming in Python - Part 1 - Basic concepts](https://www.youtube.com/watch?v=1Jc41dIVOk8&list=PLWtCrYLGt7T3DUFPYdqrdEqzt-OCfBQ5O&index=2&t=0s)
* Video [Object-oriented programming in Python - Part 2 - Types and classes in Python](https://www.youtube.com/watch?v=-O7OHmrQMfc&list=PLWtCrYLGt7T3DUFPYdqrdEqzt-OCfBQ5O&index=3&t=0s)
* Video [Object-oriented programming in Python - Part 3 - How to create a class](https://www.youtube.com/watch?v=_b18YfmEvTg&list=PLWtCrYLGt7T3DUFPYdqrdEqzt-OCfBQ5O&index=4&t=0s)
* Video [Object-oriented programming in Python - Part 4 - Classes and instances](https://www.youtube.com/watch?v=6ZPXR0Gj0GU&list=PLWtCrYLGt7T3DUFPYdqrdEqzt-OCfBQ5O&index=5&t=0s)
* Video [Object-oriented programming in Python - Part 5 - Delegation: inheritance and composition](https://www.youtube.com/watch?v=FLwxJrwYHbo&list=PLWtCrYLGt7T3DUFPYdqrdEqzt-OCfBQ5O&index=6&t=0s)

To get more comfortable with classes and delegation you can review them with these two simple articles:

* [Accessing attributes in Python]({filename}accessing-attributes-in-python.mau)
* [Method overriding in Python]({filename}method-overriding-in-python.mau)

The following posts are simple but very useful exercises of OOP design, and they also use test-driven development (TDD)

* [A simple example of Python OOP development (with TDD) - Part 1]({filename}python-oop-tdd-example-part1.mau) and [A simple example of Python OOP development (with TDD) - Part 2]({filename}python-oop-tdd-example-part2.mau)
* [Refactoring with tests in Python: a practical example]({filename}refactoring-with-tests-in-python-a-practical-example.markdown)

Speaking of TDD, you should get used to mocks, that are extremely useful when it comes to test objects that work together:

* [Python Mocks: a gentle introduction - Part 1]({filename}python-mocks-a-gentle-introduction-part-1.markdown) and [Python Mocks: a gentle introduction - Part 2]({filename}python-mocks-a-gentle-introduction-part-2.markdown)

Before you move on to more complex concepts you might want to have a look at what you can build with these basic concepts of OOP and TDD:

* [Clean architectures in Python: a step-by-step example]({filename}clean-architectures-in-python-a-step-by-step-example.mau) which evolved in my book "Clean Architectures in Python" available [online](https://www.thedigitalcatbooks.com/pycabook-introduction/) or [as PDF/ebook](https://leanpub.com/clean-architectures-in-python)

If you made it so far you are ready to face the big players! The following are concepts for advanced programmers, so be sure you are comfortable with the previous sections before venturing here. Don't be too scared, though. These concepts are advanced but not impossible, and if you want to get better at Python you should at least understand what they are.

* Metaclasses: [Object-Oriented Programming in Python 3 - Metaclasses]({filename}python-3-oop-part-5-metaclasses.markdown) and [Advanced use of Python decorators and metaclasses]({filename}decorators-and-metaclasses.markdown)
* Abstract Base Classes: [Object-Oriented Programming in Python 3 - Abstract Base Classes]({filename}python-3-oop-part-6-abstract-base-classes.markdown) and [Python decorators: metaprogramming with style]({filename}python-decorators-metaprogramming-with-style.markdown)
* Multiple inheritance: [Multiple inheritance and mixin classes in Python]({filename}multiple-inheritance-and-mixin-classes-in-python.mau)

## Final words

Wow, this was a nice trip! Obviously, I don't think I covered all that can be said on OOP in Python, but I'm pretty sure that all these posts can boost your knowledge of this important paradigm and allow you to become a better Python programmer. I will update this post when I will publish new articles on OOP here on the blog.

If you found this useful, please consider sharing it with a friend! 

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/blog_source/issues) page is the best place to submit corrections.

Image by <a href="https://pixabay.com/users/LoggaWiggler-15/?utm_source=link-attribution&amp;utm_medium=referral&amp;utm_campaign=image&amp;utm_content=5236">LoggaWiggler</a> from <a href="https://pixabay.com/?utm_source=link-attribution&amp;utm_medium=referral&amp;utm_campaign=image&amp;utm_content=5236">Pixabay</a>
