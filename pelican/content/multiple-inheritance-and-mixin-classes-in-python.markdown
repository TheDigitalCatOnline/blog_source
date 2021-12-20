Title: Multiple inheritance and mixin classes in Python
Date: 2020-03-27 12:00:00 +0100
Modified: 2020-04-08 12:00:00 +0100
Category: Programming
Tags: algorithms, Django, OOP, Python, Python3
Authors: Leonardo Giordani
Slug: mixin-classes-in-python
Image: multiple-inheritance-and-mixin-classes-in-python
Summary: This post describes what mixin classes are in theory, why we need them and how they can be implemented in Python. It also shows a working example from the class-based views code of the Django framework.

I recently revisited three old posts on Django class-based views that I wrote for this blog, updating them to Django 3.0 (you can find them [here]({filename}digging-up-django-class-based-views-1.markdown)) and noticed once again that the code base uses _mixin classes_ to increase code reuse. I also realised that mixins are not very popular in Python, so I decided to explore them, brushing up my knowledge of the OOP theory in the meanwhile.

To fully appreciate the content of the post, be sure you grasp two pillars of the OOP approach: **delegation**, in particular how it is implemented through inheritance, and **polymorphism**. [This post about delegation]({filename}python-3-oop-part-3-delegation-composition-and-inheritance.markdown) and [this post about polymorphism]({filename}python-3-oop-part-4-polymorphism.markdown) contain all you need to understand how Python implements those concepts.

## Multiple inheritance: blessing and curse

### General concepts

To discuss mixins we need to start from one of the most controversial subjects in the whole OOP world: multiple inheritance. This is a natural extension of the concept of simple inheritance, where a class automatically delegates method and attribute resolution to another class (the parent class).

Let me state it again, as it is important for the rest of the discussion: _inheritance is just an automatic delegation mechanism_.

Delegation was introduced in OOP as a way to reduce code duplication. When an object needs a specific feature it just delegates it to another class (either explicitly or implicitly), so the code is written just once.

Let's consider the example of code management website, clearly completely fictional and not inspired by any existing product. Let's assume we created the following hierarchy

``` text
      assignable reviewable item
 (assign_to_user, ask_review_to_user)
                 ^
                 |
                 |
                 |
            pull request
```

which allows us to put in `pull request` only the specific code required by that element. This is a great achievement, as it is what libraries do for code, but on live objects. Method calls and delegation are nothing more than messages between objects, so the delegation hierarchy is just a simple networked system.

Unfortunately, the use of inheritance over composition often leads to systems that, paradoxically, increase code duplication. The main problem lies in the fact that inheritance can directly delegate to only one other class (the parent class), as opposed to composition, where the object can delegate to any number of other ones. This limitation of inheritance means that we might have a class that inherits from another one because it needs some of its features, but doing this receives features it doesn't want, or shouldn't have.

Let's continue the example of the code management portal, and consider an `issue`, which is an item that we want to store in the system, but cannot be reviewed by a user. If we create a hierarchy like this

``` text
      assignable reviewable item
   (assign_to_user, ask_review_to_user)
                   ^
                   |
                   |
                   |
                   |
          +--------+--------+
          |                 |
          |                 |
          |                 |
        issue          pull request
   (not reviewable)
```

we end up putting the features related to the review process in an object that shouldn't have them. The standard solution to this problem is that of increasing the depth of the inheritance hierarchy and to derive from the new simpler ancestor.

``` text
          assignable item
         (assign_to_user)
                 ^
                 |
                 |
                 |
                 |
          +------+--------------+
          |                     |
          |                     |
          |                     |
          |         reviewable assignable item
          |            (ask_review_to_user)
          |                     ^
          |                     |
          |                     |
          |                     |
        issue              pull request
```

However, this approach stops being viable as soon as an object needs to inherit from a given class but not from the parent of that class. For example, an element that has to be reviewable but not assignable, like a `best practice` that we want to add to the site. If we want to keep using inheritance, the only solution at this point is to duplicate the code that implements the reviewable nature of the item (or the code that implements the assignable feature) and create two different class hierarchies.

``` text
          assignable item              +-------->  reviewable item
         (assign_to_user)              |         (ask_review_to_user)
                 ^                     |                  ^
                 |                     |                  |
                 |                     |                  |
                 |             CODE DUPLICATION           |
                 |                     |                  |
          +------+--------------+      |                  |
          |                     |      |                  |
          |                     |      |                  |
          |                     |      V                  |
          |         reviewable assignable item            |
          |            (ask_review_to_user)               |
          |                     ^                         |
          |                     |                         |
          |                     |                         |
          |                     |                         |
        issue              pull request             best practice
```

Please note that this doesn't even take into account that the new `reviewable item` might need attributes from `assignable item`, which prompts for another level of depth in the hierarchy, where we isolate those features in a more generic class. So, unfortunately, chances are that this is only the first of many compromises we will have to accept to keep the system in a stable state if we can't change our approach.

Multiple inheritance was then introduced in OOP, as it was clear that an object might want to delegate certain actions to a given class, and other actions to a different one, mimicking what life forms do when they inherit traits from multiple ancestors (parents, grandparents, etc.).

The above situation can then be solved having `pull request` inherit from both the class that provides the assign feature and from the one that implements the reviewable nature. 

``` text
          assignable item                          reviewable item
         (assign_to_user)                        (ask_review_to_user)
                 ^                                      ^  ^
                 |                                      |  |
                 |                                      |  |
                 |                                      |  |
                 |                                      |  |
          +------+-------------+ +----------------------+  |
          |                    | |                         |
          |                    | |                         |
          |                    | |                         |
          |                    | |                         |
          |                    | |                         |
          |                    | |                         |
          |                    | |                         |
          |                    | |                         |
          |                    | |                         |
        issue              pull request              best practice
```

Generally speaking, then, multiple inheritance is introduced to give the programmer a way to keep using inheritance without introducing code duplication, keeping the class hierarchy simpler and cleaner. Eventually, everything we do in software design is to try and separate concerns, that is, to isolate features, and multiple inheritance can help to do this.

These are just examples and might be valid or not, depending on the concrete case, but they clearly show the issues that we can have even with a very simple hierarchy of 4 classes. Many of these problems clearly arise from the fact that we wanted to implement delegation only through inheritance, and I dare to say that 80% of the architectural errors in OOP projects come from using inheritance instead of composition and from using god objects, that is classes that have responsibilities over too many different parts of the system. Always remember that OOP was born with the idea of small objects interacting through messages, so the considerations we make for monolithic architectures are valid even here.

That said, as inheritance and composition implement two different types of delegation (_to be_ and _to have_), they are both valuable, and multiple inheritance is the way to remove the single provider limitation that comes from having only one parent class.

### Why is it controversial?

Given what I just said, multiple inheritance seems to be a blessing. When an object can inherit from multiple parents, we can easily spread responsibilities among different classes and use only the ones we need, promoting code reuse and avoiding god objects.

Unfortunately, things are not that simple. First of all, we face the issue that every microservice-oriented architecture faces, that is the risk of going from god objects (the extreme monolithic architecture) to almost empty objects (the extreme distributed approach), burdening the programmer with too a fine-grained control that eventually results in a system where relationships between objects are so complicated that it becomes impossible to grasp the effect of a change in the code.

There is a more immediate problem in multiple inheritance, though. As it happens with the natural inheritance, parents can provide the same "genetic trait" in two different flavours, but the resulting individual will have only one. Leaving aside genetics (which is incredibly more complicated than programming) and going back to OOP, we face a problem when an object inherits from two other objects that provide the same attribute.

So, if your class `Child` inherits from parents `Parent1` and `Parent2`, and both provide the `__init__` method, which one should your object use?

``` python
class Parent1():
	def __init__(self):
		[...]


class Parent2():
	def __init__(self):
		[...]


class Child(Parent1, Parent2):
	# This inherits from both Parent1 and Parent2, which __init__ does it use?
	pass
```

Things can even get worse, as parents can have different signatures of the common method, for example

``` python
class Parent1:
	# This inherits from Ancestor but redefines __init__
	def __init__(self, status):
		[...]


class Parent2:
	# This inherits from Ancestor but redefines __init__
	def __init__(self, name):
		[...]


class Child(Parent1, Parent2):
	# This inherits from both Parent1 and Parent2, which __init__ does it use?
	pass
```

The problem can be extended even further, introducing a common ancestor above `Parent1` and `Parent2`.

``` python
class Ancestor:
	# The common ancestor, defines its own __init__ method
	def __init__(self):
		[...]


class Parent1(Ancestor):
	# This inherits from Ancestor but redefines __init__
	def __init__(self, status):
		[...]


class Parent2(Ancestor):
	# This inherits from Ancestor but redefines __init__
	def __init__(self, name):
		[...]


class Child(Parent1, Parent2):
	# This inherits from both Parent1 and Parent2, which __init__ does it use?
	pass
```

As you can see, we already have a problem when we introduce multiple parents, and a common ancestor just adds a new level of complexity. The ancestor class can clearly be at any point of the inheritance tree (grandparent, grand-grandparent, etc.), the important part is that it is shared between `Parent1` and `Parent2`. This is the so-called diamond problem, as the inheritance graph has the shape of a diamond

``` text
      Ancestor
       ^   ^
      /     \
     /       \
Parent1     Parent2
    ^         ^
     \       /
      \     /
       Child
```

So, while with single-parent inheritance the rules are straightforward, with multiple inheritance we immediately have a more complex situation that doesn't have a trivial solution. Does all this prevent multiple inheritance from being implemented?

Not at all! There are solutions to this problem, as we will see shortly, but this further level of intricacy makes multiple inheritance something that doesn't fit easily in a design and has to be implemented carefully to avoid subtle bugs. Remember that inheritance is an automatic delegation mechanism, as this makes what happens in the code less evident. For these reasons, multiple inheritance is often depicted as scary and convoluted, and usually given some space only in the advanced OOP courses, at least in the Python world. I believe every Python programmer, instead, should familiarise with it and learn how to take advantage of it.

### Multiple inheritance: the Python way

Let's see how it is possible to solve the diamond problem. Unlike genetics, we programmers can't afford any level of uncertainty or randomness in our processes, so in the presence of a possible ambiguity as the one created by multiple inheritance, we need to write down a rule that will be strictly followed in every case. In Python, this rule goes by the name of MRO (Method Resolution Order), which was introduced in Python 2.3 and is described in [this document](https://www.python.org/download/releases/2.3/mro/) by Michele Simionato.

There is a lot to say about MRO and the underlying C3 linearisation algorithm, but for the scope of this post, it is enough to see how it solves the diamond problem. In case of multiple inheritance, Python follows the usual inheritance rules (automatic delegation to an ancestor if the attribute is not present locally), but the _order_ followed to traverse the inheritance tree now includes all the classes that are specified in the class signature. In the example above, Python would look for attributes in the following order: `Child`, `Parent1`, `Parent2`, `Ancestor`.

So, as in the case of standard inheritance, this means that the first class in the list that implements a specific attribute will be the selected provider for that resolution. An example might clarify the matter

``` python
class Ancestor:
    def rewind(self):
        print("Ancestor: rewind")


class Parent1(Ancestor):
    def open(self):
        print("Parent1: open")


class Parent2(Ancestor):
    def open(self):
        print("Parent2: open")

    def close(self):
        print("Parent2: close")

    def flush(self):
        print("Parent2: flush")


class Child(Parent1, Parent2):
    def flush(self):
        print("Child: flush")


print(Child.__mro__)

c = Child()
c.rewind()
c.open()
c.close()
c.flush()

```

As you can see, we can access the MRO of any class reading its `__mro__` attribute, and as we expected its value is `(<class '__main__.Child'>, <class '__main__.Parent1'>, <class '__main__.Parent2'>, <class '__main__.Ancestor'>, <class 'object'>)`.


So, in this case an instance `c` of `Child` provides `rewind`, `open`, `close`, and `flush`. When `c.rewind` is called, the code in `Ancestor` is executed, as this is the first class in the MRO list that provides that method. The method `open` is provided by `Parent1`, while `close` is provided by `Parent2`. If the method `c.flush` is called, the code is provided by the `Child` class itself, that redefines it overriding the one provided by `Parent2`.

As we see with the `flush` method, Python doesn't change its behaviour when it comes to method overriding with multiple parents. The first implementation of a method with that name is executed, and the parent's implementation is not automatically called. As in the case of standard inheritance, then, it's up to us to design classes with matching method signatures.

#### Under the bonnet

How does multiple inheritance work internally? How does Python create the MRO list?

Python has a very simple approach to OOP (even though it ultimately ends with a mind-blowing ouroboros, see [here]({filename}python-3-oop-part-5-metaclasses.markdown)). Classes are objects themselves, so they contain data structures that are used by the language to provide features, and delegation makes no exception. When we run a method on an object, Python silently uses the `__getattribute__` method (provided by `object`), which uses `__class__` to reach the class from the instance, and `__bases__` to find the parent classes. The latter, in particular, is a tuple, so it is ordered, and it contains all the classes that the current class inherits from.

The MRO is created using only `__bases__`, but the underlying algorithm is not that trivial and has to with the monotonicity of the resulting class linearisation. It is less scary than it sounds, but not something you want to read while suntanning, probably. If that's the case, the aforementioned [document](https://www.python.org/download/releases/2.3/mro/) by Michele Simionato contains all the gory details on class linearisation that you always wanted to explore while lying on the beach.

## Inheritance and interfaces

To approach mixins, we need to discuss inheritance in detail, and specifically the role of method signatures.

In Python, when you override a method provided by an ancestor class, you have to decide if and when to call its original implementation. This gives the programmer the freedom to decide whether they need to just augment a method or to replace it completely. Remember that the only thing Python does when a class inherits from another is to automatically delegate methods that are not implemented.

When a class inherits from another we are ideally creating objects that keep the backward compatibility with the interface of the parent class, to allow a polymorphic use of them. This means that when we inherit from a class and override a method changing its signature we are doing something that is dangerous and, at least from the point of view of polymorphism, wrong. Have a look at this example

``` python
class GraphicalEntity:
    def __init__(self, pos_x, pos_y, size_x, size_y):
        self.pos_x = pos_x
        self.pos_y = pos_y
        self.size_x = size_x
        self.size_y = size_y

    def move(self, pos_x, pos_y):
        self.pos_x = pos_x
        self.pos_y = pos_y

    def resize(self, size_x, size_y):
        self.size_x = size_x
        self.size_y = size_y
        

class Rectangle(GraphicalEntity):
    pass
    

class Square(GraphicalEntity):
    def __init__(self, pos_x, pos_y, size):
        super().__init__(pos_x, pos_y, size, size)

    def resize(self, size):
        super().resize(size, size)
```

Please note that `Square` changes the signature of both `__init__` and `resize`. Now, when we instantiate those classes we need to keep in mind the different signature of `__init__` in `Square`

``` python
r1 = Rectangle(100, 200, 15, 30)
r2 = Rectangle(150, 280, 23, 55)
q1 = Square(300, 400, 50)
```

We usually accept that an enhanced version of a class accepts different parameters when it is initialised, as we do not expect it to be polymorphic on `__init__`. Problems arise when we try to leverage polymorphism on other methods, for example resizing all `GraphicalEntity` objects in a list

``` python
for shape in [r1, r2, q1]:
    size_x = shape.size_x
    size_y = shape.size_y
    shape.resize(size_x*2, size_y*2)
```

Since `r1`, `r2`, and `q1` are all objects that inherit from `GraphicalEntity` we expect them to provide the interface provided by that class, but this fails, because `Square` changed the signature of `resize`. The same would happen if we instantiated them in a for loop from a list of classes, but as I said it is generally accepted that child classes change the signature of the `__init__` method. This is not true, for example, in a plugin-based system, where all plugins shall be initialised the same way.

This is a classic problem in OOP. While we, as humans, perceive a square just as a slightly special rectangle, from the interface point of view the two classes are different, and thus should not be in the same inheritance tree when we are dealing with dimensions. This is an important consideration: `Rectangle` and `Square` are polymorphic on the `move` method, but not on `__init__` and `resize`. So, the question is if we could somehow separate the two natures of being movable and resizeable.

Now, discussing interfaces, polymorphism, and the reasons behind them would require an entirely separate post, so in the following sections, I'm going to ignore the matter and just consider the object interface optional. You will thus find examples of objects that break the interface of the parent, and objects that keep it. Just remember: whenever you change the signature of a method you change the (implicit) interface of the object, and thus you stop polymorphism. I'll discuss another time if I consider this right or wrong.

## Mixin classes

MRO is a good solution that prevents ambiguity, but it leaves programmers with the responsibility of creating sensible inheritance trees. The algorithm helps to resolve complicated situations, but this doesn't mean we should create them in the first place. So, how can we leverage multiple inheritance without creating systems that are too complicated to grasp? Moreover, is it possible to use multiple inheritance to solve the problem of managing the double (or multiple) nature of an object, as in the previous example of a movable and resizeable shape?

The solution comes from mixin classes: those are small classes that provide attributes but are not included in the standard inheritance tree, working more as "additions" to the current class than as proper ancestors. Mixins originate in the LISP programming language, and specifically in what could be considered the first version of the Common Lisp Object System, the Flavors extension. Modern OOP languages implement mixins in many different ways: Scala, for example, has a feature called _traits_, which live in their own space with a specific hierarchy that doesn't interfere with the proper class inheritance.

### Mixin classes in Python

Python doesn't provide support for mixins with any dedicated language feature, so we use multiple inheritance to implement them. This clearly requires great discipline from the programmer, as it violates one of the main assumptions for mixins: their orthogonality to the inheritance tree. In Python, so-called mixins are classes that live in the normal inheritance tree, but they are kept small to avoid creating hierarchies that are too complicated for the programmer to grasp. In particular, mixins shouldn't have common ancestors other than `object` with the other parent classes.

Let's have a look at a simple example

``` python
class GraphicalEntity:
    def __init__(self, pos_x, pos_y, size_x, size_y):
        self.pos_x = pos_x
        self.pos_y = pos_y
        self.size_x = size_x
        self.size_y = size_y


class ResizableMixin:
    def resize(self, size_x, size_y):
        self.size_x = size_x
        self.size_y = size_y

        
class ResizableGraphicalEntity(GraphicalEntity, ResizableMixin):
    pass

rge = ResizableGraphicalEntity(5, 4, 200, 300)
rge.resize(1000, 2000)
```

Here, the class `ResizableMixin` doesn't inherit from `GraphicalEntity`, but directly from `object`, so `ResizableGraphicalEntity` gets from it just the `resize` method. As we said before, this simplifies the inheritance tree of `ResizableGraphicalEntity` and helps to reduce the risk of the diamond problem. It leaves us free to use `GraphicalEntity` as a parent for other classes without having to inherit methods that we don't want. Please remember that this happens because the classes are designed to avoid it, and not because of language features: the MRO algorithm just ensures that there will always be an unambiguous choice in case of multiple ancestors.

Mixins cannot usually be too generic. After all, they are designed to add features to classes, but these new features often interact with other pre-existing features of the augmented class. In this case, the `resize` method interacts with the attributes `size_x` and `size_y` that have to be present in the object. Obviously, there are obviously examples of _pure_ mixins, but since they would require no initialization their scope is definitely limited.

### Using mixins to hijack inheritance

Thanks to the MRO, Python programmers can leverage multiple inheritance to override methods that objects inherit from their parents, allowing them to customise classes without code duplication. Let's have a look at this example

``` python
class GraphicalEntity:
    def __init__(self, pos_x, pos_y, size_x, size_y):
        self.pos_x = pos_x
        self.pos_y = pos_y
        self.size_x = size_x
        self.size_y = size_y

class Button(GraphicalEntity):
    def __init__(self, pos_x, pos_y, size_x, size_y):
        super().__init__(pos_x, pos_y, size_x, size_y)
        self.status = False

    def toggle(self):
        self.status = not self.status

b = Button(10, 20, 200, 100)
```

As you can see the `Button` class extends the `GraphicalEntity` one in a classic way, using `super` to call the parent's `__init__` method before adding the new `status` attribute. Now, if I wanted to create a `SquareButton` class I have two choices.

I might just override `__init__` in the new class

``` python
class GraphicalEntity:
    def __init__(self, pos_x, pos_y, size_x, size_y):
        self.pos_x = pos_x
        self.pos_y = pos_y
        self.size_x = size_x
        self.size_y = size_y


class Button(GraphicalEntity):
    def __init__(self, pos_x, pos_y, size_x, size_y):
        super().__init__(pos_x, pos_y, size_x, size_y)
        self.status = False

    def toggle(self):
        self.status = not self.status


class SquareButton(Button):
    def __init__(self, pos_x, pos_y, size):
        super().__init__(pos_x, pos_y, size, size)

b = SquareButton(10, 20, 200)
```

which performs the requested job, but strongly connects the feature of having a single dimension with the `Button` nature. If we wanted to create a circular image we could not inherit from `SquareButton`, as the image has a different nature.

The second option is that of isolating the features connected with having a single dimension in a mixin class, and add it as a parent for the new class

``` python
class GraphicalEntity:
    def __init__(self, pos_x, pos_y, size_x, size_y):
        self.pos_x = pos_x
        self.pos_y = pos_y
        self.size_x = size_x
        self.size_y = size_y


class Button(GraphicalEntity):
    def __init__(self, pos_x, pos_y, size_x, size_y):
        super().__init__(pos_x, pos_y, size_x, size_y)
        self.status = False

    def toggle(self):
        self.status = not self.status


class SingleDimensionMixin:
    def __init__(self, pos_x, pos_y, size):
        super().__init__(pos_x, pos_y, size, size)


class SquareButton(SingleDimensionMixin, Button):
    pass

b = SquareButton(10, 20, 200)
```

The second solution gives the same final result, but promotes code reuse, as now the `SingleDimensionMixin` class can be applied to other classes derived from `GraphicalEntity` and make them accept only one size, while in the first solution that feature was tightly connected with the `Button` ancestor class.

Please note that the position of the mixin is important as `super` follows the MRO. As it is, the MRO of `SquareButton` is `(SquareButton, SingleDimensionMixin, Button, GraphicalEntity, object)`, so, when we instantiate it the `__init__` method is provided by `SingleDimensionMixin`, which in turn calls through `super` the method `__init__` of `Button`. The call `super().__init__(pos_x, pos_y, size, size)` in `SingleDimensionMixin` and the signature `def __init__(self, pos_x, pos_y, size_x, size_y):` in `Button` match, so everything works.

If we defined `SquareButton` as

``` python
class SquareButton(Button, SingleDimensionMixin):
    pass
```

then the `__init__` method would first be provided by `Button`, and its `super` would call the `__init__` method of `GraphicalEntity`. This would however result in an error, as we run `SquareButton(10, 20, 200)`, and `Button.__init__` expects four parameters.

Mixins are not used only when you want to change the object's interface, though. Leveraging `super` we can achieve interesting designs like

``` python
class GraphicalEntity:
    def __init__(self, pos_x, pos_y, size_x, size_y):
        self.pos_x = pos_x
        self.pos_y = pos_y
        self.size_x = size_x
        self.size_y = size_y


class Button(GraphicalEntity):
    def __init__(self, pos_x, pos_y, size_x, size_y):
        super().__init__(pos_x, pos_y, size_x, size_y)
        self.status = False

    def toggle(self):
        self.status = not self.status


class LimitSizeMixin:
    def __init__(self, pos_x, pos_y, size_x, size_y):
        size_x = min(size_x, 500)
        size_y = min(size_y, 400)
        super().__init__(pos_x, pos_y, size_x, size_y)


class LimitSizeButton(LimitSizeMixin, Button):
    pass

b = LimitSizeButton(10, 20, 2000, 1000)
print(b.size_x)
print(b.size_y)
```

Here, the MRO or `LimitSizeButton` is `(<class '__main__.LimitSizeButton'>, <class '__main__.LimitSizeMixin'>, <class '__main__.Button'>, <class '__main__.GraphicalEntity'>, <class 'object'>)`, which means that when we initialize it the `__init__` method is first provided by `LimitSizeMixin`, which then calls through `super` the `__init__` method of `Button`, and through the latter the `__init__` method of `GraphicalEntity`.

Remember that in Python, you are never forced to call the parent's implementation of a method, so the mixin here might also stop the dispatching mechanism if that is the requirement of the business logic of the new object.

## A real example: Django class-based views

Finally, let's get to the original source of inspiration for this post: the Django codebase. I will show you here how the Django programmers used multiple inheritance and mixin classes to promote code reuse, and you will now hopefully grasp all the reasons behind them.

The example I chose can be found in the [code of generic views](https://github.com/django/django/blob/3.0/django/views/generic/base.py#L117), and in particular in two classes: `TemplateResponseMixin` and `TemplateView`.

As you might know, Django `View` class is the ancestor of all class-based views and provides a `dispatch` method that converts HTTP request methods into Python function calls ([CODE](https://github.com/django/django/blob/3.0/django/views/generic/base.py#L89)). Now, the `TemplateView` is a view that answers to a GET request rendering a template with the data coming from a context passed when the view is called. Given the mechanism behind Django views, then, `TemplateView` should implement a `get` method and return the content of the HTTP response. The code of the class is

``` python
class TemplateView(TemplateResponseMixin, ContextMixin, View):
    """
    Render a template. Pass keyword arguments from the URLconf to the context.
    """
    def get(self, request, *args, **kwargs):
        context = self.get_context_data(**kwargs)
        return self.render_to_response(context)
```

As you can see `TemplateView` is a `View`, but it uses two mixins to inject features. Let's have a look at `TemplateResponseMixin`

``` python
class TemplateResponseMixin:
    [...]

    def render_to_response(self, context, **response_kwargs):
        [...]

    def get_template_names(self):
        [...]
```

[I removed the code of the class as it is not crucial for the present discussion, you can see the full class [here](https://github.com/django/django/blob/3.0/django/views/generic/base.py#L117)]

It is clear that `TemplateResponseMixin` just adds to any class the two methods `get_template_names` and `render_to_response`. The latter is called in the `get` method of `TemplateView` to create the response. Let's have a look at a simplified schema of the calls:

``` text
GET request --> TemplateView.dispatch --> View.dispatch --> TemplateView.get --> TemplateResponseMixin.render_to_response
```

It might look complicated, but try to follow the code a couple of times and the whole picture will start to make sense. The important thing I want to stress is that the code in `TemplateResponseMixin` is available for any class that wants to have the feature of rendering a template, for example `DetailView` ([CODE](https://github.com/django/django/blob/3.0/django/views/generic/detail.py#L164)), which receives the feature of showing the details of a single object by `SingleObjectTemplateResponseMixin`, which inherits from `TemplateResponseMixin`, overriding its method `get_template_names` ([CODE](https://github.com/django/django/blob/3.0/django/views/generic/detail.py#L111)).

As we discussed before, mixins cannot be too generic, and here we see a good example of a mixin designed to work on specific classes. `TemplateResponseMixin` has to be applied to classes that contain `self.request` ([CODE](https://github.com/django/django/blob/3.0/django/views/generic/base.py#L133)), and while this doesn't mean exclusively classes derived from `View`, it is clear that it has been designed to augment that specific type.

## Takeaway points

* Inheritance is designed to promote code reuse but can lead to the opposite result
* Multiple inheritance allows us to keep the inheritance tree simple
* Multiple inheritance leads to possible problems that are solved in Python through the MRO
* Interfaces (either implicit or explicit) should be part of your design
* Mixin classes are used to add simple changes to classes
* Mixins are implemented in Python using multiple inheritance: they have great expressive power but require careful design.

## Final words

I hope this post helped you to understand a bit more how multiple inheritance works, and to be less scared by it. I also hope I managed to show you that classes have to be carefully designed and that there is a lot to consider when you create a class system. Once again, please don't forget composition, it's a powerful and too often forgotten tool.

## Updates

2020-03-13: GitHub user [sureshvv](https://github.com/sureshvv) noticed that the `LimitSizeMixin` method `__init__` had the wrong parameters `pos_x` and `pos_y`, instead of `size_x` and `size_y`. Thanks!
2021-12-20: [Alexander](https://github.com/akocur) fixed a mistake in the part relative to `SquareButton` and the behaviour of `super()`. Thanks!

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.

