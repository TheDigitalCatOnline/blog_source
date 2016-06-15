Title: Clean architectures in Python: a step-by-step example
Date: 2016-06-13 15:00:00 +0100
Category: Programming
Tags: OOP, Python, Python2, Python3, TDD, architectures
Authors: Leonardo Giordani
Slug: clean-architectures-in-python-a-step-by-step-example
Summary: 

I was recently being introduced by my collegue [Roberto Ciatti](https://github.com/gekorob) to the concept of Clean Architecture, as called and strongly suggested by Robert Martin. The well-known Uncle Bob has been talking about this concept at conferences and wrote many posts about it. What he calls "Clean Architecture" is a way of structuring a software system, a set of consideration (more than strict rules) about the different layers and the role of the actors in it.

As he clearly states in a post aptly titled [The Clean Architecture](https://blog.8thlight.com/uncle-bob/2012/08/13/the-clean-architecture.html), the idea behind this design is not new, being built on a set of concepts that have been pushed by many software engineers over the last 3 decades. One of the first implementations may be found in the Boundary-Control-Entity model proposed by Ivan Jacobson in his masterpiece "Object-Oriented Software Engineering: A Use Case Driven Approach" published in 1992, but Martin lists other more recent versions of this architecture.

I will not repeat here what he had already explained better than I can do, so I will just point out some resources you may check to start exploring these concepts:

* [The Clean Architecture](https://blog.8thlight.com/uncle-bob/2012/08/13/the-clean-architecture.html) a post by Robert Martin that concisely describes the goals of the architecture. Also lists resources that describe similar architectures.
* [The Open Closed Principle](https://blog.8thlight.com/uncle-bob/2014/05/12/TheOpenClosedPrinciple.html) a post by Robert Martin not strictly correlated with the Clean Architecture concept but important for the separation concept.
* Hakka Labs: Robert "Uncle Bob" Martin - [Architecture: The Lost Years](https://www.youtube.com/watch?v=HhNIttd87xs) a video of Robert Martin from Hakka Labs.
* [DDD & Testing Strategy](http://www.taimila.com/blog/ddd-and-testing-strategy/) by Lauri Taimila

The purpose of this series of posts is to show how to build a REST web service in Python from scratch using a clean architecture. One of the main advantages of this layered design is testability, so I will develop it following a TDD approach. This will also allow to show some limitations of the test-driven approach which you should be aware of.

The idea for this project comes from an interview assignment I received from a company and has been slightly changed to avoid spoiling the fun of potential candidates =). Indeed the project was really interesting, and was developed from scratch in around 14 hours of work. Given the nature of the project (a code test) some choices have been made to simplify the resulting code. Whenever meaningful I will point out those simplifications and discuss them.  
  
# Project overview

The goal of the project is to realize a simple search engine on top of a dataset of objects which are described by some quantities. The search engine shall implement a ranking system that shows the best results first. The implementation of the ranking system is up to the programmer and could be subject to changes.
 
The objects in the dataset are storage rooms for rent described by the following quantities:
 
* A unique code
* A size in square meters
* A renting price in euros/day
* Latitude and longitude

As pushed by the clean architecture model, we are interested in separating the different layers of the system. The architecture is described by four layers, which however can be implemented by more than four actual code modules. I will give here a brief description of those layers.

## Entities

This is the level in which the domain models are described. Since we work in Python, I will put here the class that represent my storage rooms, with the data contained in the database, and whichever data I think is useful to perform the core business processing. In this case the model will also contain ranks coming from the ranking system (which depend on the search).

It is very important to understand that the models in this layer are different from the usual models of framework like Django. These models are not connected with a storage system, so they cannot be saved or queried using methods of their classes.
  
## Use cases

This layer contains the use cases implemented by the system. In this simple example there will be only one use case, which is the list of storage rooms according to the given filters and weights. Here you would put for example a use case that shows the detail of a given storage room or every business process you want to implement, such as booking a storage room, filling it with goods, etcetera.

## Interface Adapters

This layer corresponds to two different parts of the project. The first one is the data storage, or repository. This implements the access to the data storage according to a given API, which is common among repositories. For this example no database has been involved and a file based repository wil be developed. This allows to show a very simple and self-contained repository that works without the need of external systems. Obviously such solution does not consider performance an issue, which would instead be a big concern in a production system.

The second part that belongs to this layer is the presentation, which in this case is implemented by a simple JSON serializer. 

# API and shades of grey

The word API is of uttermost importance in a clean architecture. Every layer may be accessed by an API, that is a fixed collection of entry points (methods or objects). Here "fixed" means "the same among every implementation", obviously an API may change with time. Every presentation layer, for example, will access the same use cases, and the same methods, to obtain a set of domain models, which are the output of that particular use case. It is up to the presentation layer, then to format data according to the specific presentation media, for example HTML, PDF, images, etcetera.

The same concept is valid for the storage layer. Every storage implementation shall provide the same methods. When dealing with use cases you shall not be concerned with the actual system that stores data, it may be a MongoDB local installation, a cloud storage system or a trivial in-memory dictionary. You only need to know the API of that layer, which every implementation shall expose.
   
THe separation between layers, and the content of each layer, is not always fixed and immutable. A well-designed system shall also cope with practical world issues such as performances, for example, or other specific needs. When designing an architecture it is very important to know "what is where and why", and this is even more important when you "bend" the rules. Many issues do not have a black-or-white answer, and many decisions are "shades of grey", that is it is up to you to justify why you put something there.
  
The project we are going to implement shows one of these decisions, and I want to briefly discuss it. 