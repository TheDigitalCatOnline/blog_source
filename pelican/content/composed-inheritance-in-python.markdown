Title: 
Date: 2020-07-27 15:00:00 +0100
Category: Programming, Projects, Retro
Tags: algorithms, amiga, AMQP, architectures, assembly, AWS, blogging, C, Clojure, compilers, concurrent programming, cryptography, decorators, Django, Docker, editor, Emacs, Erlang, Flask, functional programming, generators, Git, HTTP, infrastructure, JavaScript, M68000, Markdown, metaclasses, metaprogramming, Notebook, OOP, operating systems, pelican, Pika, Postage, Postgres, Python, Python2, Python3, RabbitMQ, refactoring, retroprogramming, RSA, Scala, SSH, SSL, TDD, Terraform, testing, tools, versioning, video, WWW
Authors: Leonardo Giordani
Slug: composed-inheritance-in-python
Summary: 

## Intro

Python is an OO language, has some functional languages features.
Core of OO is small objects interacting together, like microservices.
Sending messages is not different from calling functions, objects are functions with state.
Interaction pushes for code reuse, you don't reimplement if you can delegate.

## Delegation in OOP

Delegation is about using code that is in another object, which ultimately means that it is defined in a different class.
There are two way to connect objects. To have the object (or to know it) and to be.
To have means to be connected with an instance of the other class.
To be means to be connected with the other class directly.

## Dichotomy

Inheritance shares the state, composition doesn't.
Inheritance is implicit and automatic, composition is explicit.
Inheritance is canned, composition has to be implemented.



## Inheritance in Python
## Composition in Python
## Bad signs

Inheritance: large inheritance trees, need to override or delete attributes and methods
Composition: need to connect too many attributes and methods, need to pass too many arguments to methods

## Mixing the two: composed inheritance
## Final words
