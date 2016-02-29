Title: Digging up Django class-based views - 1
Date: 2013-10-28 08:43 +0200
Category: Programming
Tags: Python, Django
Authors: Leonardo Giordani
Slug: digging-up-django-class-based-views-1
Series: "Digging up Django class-based views"
Summary:

## Abstract

_This post refers to Django 1.5. Please be warned that some of the matters discussed here, some solutions or the given code can be outdated by more recent Django versions_

Django programmers that started with versions prior to 1.3 are used to deal with views as functions, and they learned how to process even complex forms in a procedural way. From the release 1.3, Django introduced class-based views (CBVs) and ported its powerful generic views to this new paradigm (class-based generic views, or CBGVs).

This change, however, has not been harmless for Django novices: the django-users mailing list and StackOverflow are full of questions about views and classes, and the official documentation on this topic is still a little unorganized. Well, open source things are always ready to be improved, aren't they? This post aims to share some thoughts and insights about CBVs, and hopefully it can also lead to an improvement of the official documentation.

I want to thank all Django developers for their work. Despite the previous criticism on documentation, this post wants to be a thanksgiving for their efforts.

Feel free to comment [here](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) or to submit issues [here](https://github.com/lgiordani/lgiordani.github.com/issues).

## What are CBVs?

Class-based views are, indeed, (Django) views based on (Python) classes. Until here, it makes sense, hopefully. This means that, to master them, you need to understand both Django views and Python classes, so let's give a quick definition of them.

A Django view is a piece of code that processes an HTTP request and returns an HTTP response. Oh yes, nothing more, nothing less.
A Python class is the implementation of the Object-Oriented concept of class in the Python language.

So a view just need to be a [callable](http://docs.python.org/2/library/functions.html#callable), and this includes functions and classes. Thus, to understand the advantages of class-based views over function-based views we shall discuss the merits of classes over functions. This latter sentence could be the title of a 10 volumes book on programming (followed by another 10 volumes book titled "Merits of functions over classes"), so I am just going to scratch the surface of the matter.

## Starting off with Python classes

The main point of a class is implementing encapsulation: they represent a way of coupling data and functions. Doing this, a class loses the dynamic essence of a procedure, which exists only while it is running, and becomes a living entity, something that sits there, caring for its data, and reacts when we call its functions (methods).

A good analogy for a class is a finite-state machine: once the class has been initialized, methods are what we use to make it move between states. If we do not call methods, the class simply waits there without complaining.

As an example, let's look at a very simple procedure that takes a list and extracts the even numbers from it.

``` python
def extract_even_numbers(alist):
    return [i for i in alist if i%2 == 0]
```

The example is very trivial, but I think there is always time to tangle up the code, so let us start with simple things. A class version of this function could be written as

``` python
class EvenExtractor(object):
    def __init__(self, alist):
        self.l = alist
        
    def extract(self):
        return [i for i in self.l if i%2 == 0]
```

Seems to be the same thing, doesn't it? Indeed the difference is subtle but remarkable. Now the extractor has two parts, the first being the initialization and the second being the actual extraction, and we can have the class in one of three states: before initialization (`e = EvenExtractor`), after initialization (`e = EvenExtractor(range(120))`), and after extraction (`l = e.extract()`. Being a finite-state machine we can interact with it before moving from a state to another. For example

``` python
e = EvenExtractor(range(120)) # State B
e.l = range(60)
e.extract() # State C
```

Well, despite the very simple example the point is: by converting the procedure to a class we obtained a rich tool that can execute its job step by step and, in general, can work in a non linear way.

## Playing with genetics

The real power of classes used as finite-state machines lies in the concept of [inheritance](http://en.wikipedia.org/wiki/Inheritance_%28object-oriented_programming%29). Inheritance is a mechanism through which a class can copy another class and then change only the parts that have to behave differently. Please note that here we talk about classes and not instances, so the copy action here refers to the structure of the class, not the actual data contained in it. Inheritance is a mean to mimic the behaviour of a class.

(Sidenote: *I am a strong supporter of an OO design principle that states "Favour composition over inheritance" (and favor "favor" over "favour" for US audience). I read too many OOP introductions that stress too much the inheritance mechanism and leave composition aside, raising a generation of OOP programmers that, instead of building systems populated by many small collaborating objects create nightmares infested by giant all-purpose things that sometimes resemble an operating system more than a system component. Given that, inheritance plays an important role in OOP, and here we find a very good example of its real benefit.*)

Let's continue the above example, first by enriching the EvenExtractor class:

``` python
class EvenExtractor(object):
    def __init__(self, alist):
        self.l = [int(elem) for elem in alist]
        
    def extract(self):
        return [i for i in self.l if i%2 == 0]
```

Now the class performs an important action in its initialization phase, converting all elements of the input list to integers, and we can happily use it in our Next Big Project™. Some days after this change we realize that we could also profitably use a class that extracts odd elements form a list. Being good object oriented programmers we write

```  python
class OddExtractor(EvenExtractor):
    def extract(self):
        return [i for i in self.l if i%2 != 0]
```

and call it a day. Indeed, through the inheritance mechanism expressed by that `(EvenExtractor)` signature of the new class, we defined something that is exactly the same thing as `EvenExtractor`, with the same methods and attributes, but with a different name. Then we changed the behaviour of the new class but only for the extraction part by [overriding the method](http://en.wikipedia.org/wiki/Method_overriding).

To summarize the lesson: using classes and inheritance you can build finite-state machines that are easily customizable to suit your exact needs. This obviously is just one of the many points of view under which you can consider classes, but it is the one we need to understand Django CBVs.

## Back to Django

Finally! You couldn't take Python classes anymore, could you? Sorry, we are dealing with them further, but for the moment let us move to Django to look at a practical use of what we learned in the previous sections.

A Django view is a perfect example of a finite-state machine. It takes an incoming request and makes it flow through different processing steps until a final response is produced, which is then sent back to the user. So, for the love of definitions, say that CBVs are the Django mechanism to allow the programmer to write their views leveraging the tools made available by the object-oriented paradigm. In this context (pun intended) CBGVs are the "batteries included" of Django views: (class-based) generic views are powerful tools that the framework gives you to accomplish the most usual tasks, and sometimes even the unusual ones.

Let's dig into one of the examples of the official Django docs; [here](https://docs.djangoproject.com/en/1.5/ref/class-based-views/generic-display/#listview) you find the API of the beloved `ListView`, a generic view to deal with a list of things (extracted from the database).

``` python
from django.views.generic.list import ListView
from articles.models import Article

class ArticleListView(ListView):
    model = Article
```

This example assumes that `articles` is your application and `Article` is one of its models.

This short piece of code leverages the full power of inheritance. Indeed we just derived `ArticleListView` from `ListView` and changed the class attribute `model`: how can this work? How can this class process incoming requests and what are the outputs? The official documentation states "While this view is executing, self.object_list will contain the list of objects (usually, but not necessarily a queryset) that the view is operating upon."; this leaves many dark corners, however, and if you are a novice, chances are that you are already lost.

Since `ArticleListView` derives from `ListView`, the latter is the class we have to analyze to understand how data are processed. To do this you need to look at the [documentation](https://docs.djangoproject.com/en/1.5/ref/class-based-views/generic-display/#django.views.generic.list.ListView), and if something is still unclear you can freely look at the [source code](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/list.py). As already said I find a little difficult to find a clear way through the docs, so I'll try to summarize here what I learned. In the following paragraphs you will find links like ([SC]()) which point to the source code, if you want to read it by yourself.

#### URL dispatchers and views

A CBV cannot directly be used in your URL dispatcher; instead you have to give the result of the `as_view()` method ([SC](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L31)), which basically defines a function that instances the class ([SC](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L46)) and calls the `dispatch()` method ([SC](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L47)); then the function is returned to be used in the URL dispatcher ([SC](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L55)). As a user, we are interested only in the fact that the _entry point_ of the class, i.e. the method called when a request hits the URL linked with it, is `dispatch()`.

Let's use this knowledge to print out a string on the console each time a request is served by our CBV. I'm running through this (indeed simple) task step by step since it shows exactly how you have to deal with CBVs when solving real problems.

If we define the `ArticleListView` class this way

``` python
from django.views.generic.list import ListView
from articles.models import Article

class ArticleListView(ListView):
    model = Article
	
	def dispatch(self, request, *args, **kwargs):
		return super(ArticleListView, self).dispatch(request, *args, **kwargs)
```

the class does not change its behaviour. What we did was to override the `dispatch()` method with a call to the parent's method, i.e. we explicitly wrote what Python does by default. You can find detailed information about `super()` [here](http://docs.python.org/2/library/functions.html#super). Please be also sure to understand the star and double star notation to define variable number of arguments; the official documentation is [here](http://docs.python.org/2.7/tutorial/controlflow.html#more-on-defining-functions).

Since views are automatically called by the framework, this latter expects them to comply with a very specific API, so when overriding a method you have to obey its signature. The signature of `dispatch()` can be found [here](https://docs.djangoproject.com/en/1.5/ref/class-based-views/base/#django.views.generic.base.View.dispatch); by the way, this documentation of `dispatch()` gives you good hints about the internal working mechanism of CBVs.

The `dispatch()` method receives a `request` argument, which type is `HttpRequest` ([documentation](https://docs.djangoproject.com/en/1.5/ref/request-response/#httprequest-objects)), and we can print it on the console with the standard `print()` function

``` python
from django.views.generic.list import ListView
from articles.models import Article

class ArticleListView(ListView):
    model = Article
	
	def dispatch(self, request, *args, **kwargs):
		print(request)
		return super(ArticleListView, self).dispatch(request, *args, **kwargs)
```

This prints the content of the `request` object on the standard output of the Python code, that is on the standard output of the server that is running the Django project. If you are running the Django development server, you will find the output on the text console where you issued the `django-admin.py runserver` command (or `manage.py runserver`).

This, in a nutshell, is the standard way of dealing with framework classes, and thus with Django's CBGVs: inherit from a predefined class, identify which methods you need to change, override them complying with their signature and calling the parent's code somewhere in the new code.

The full list of methods `ListView` uses when processing incoming requests is listed on its [official documentation page](https://docs.djangoproject.com/en/1.5/ref/class-based-views/generic-display/#listview) in the "Method Flowchart" section; in the "Ancestors (MRO)" section you can see that `ListView` itself inherits from a good number of other classes. MRO stands for Method Resolution Order and has to deal with multiple inheritance: if you are eager to deal with one of the most intricate Python topics feel free to read [here](http://docs.python.org/2/tutorial/classes.html#multiple-inheritance).

#### Incoming GET requests

Back to our `ArticleListView`. The `dispatch()` method of the parent reads the `method` attribute of the `request` object and selects a handler to process the request itself ([SC](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L57)): this means that if `request.method` is `'GET'`, which is the HTTP way to say that we are _reading_ a resource, `dispatch()` will call the `get()` method of the class.

The `get()` method of `ListView` comes from its `BaseListView` ancestor ([documentation](https://docs.djangoproject.com/en/1.5/ref/class-based-views/generic-display/#django.views.generic.list.BaseListView), [source code](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/list.py#L114)). This function is the one that contains the functional view code, so if you are accustomed to function-based views you'll find yourself at home here. As you can see, the function basically fills the attribute `self.object_list` with the result of the method `self.get_queryset()`, creates a context calling the method `self.get_context_data()` and calls the class version of `render_to_response()`, namely `self.render_to_response()`.

Are you still with me? Don't give up, we are nearly at the end (with ListView). The method `self.get_queryset()` comes from the `MultipleObjectMixin` ancestor of `ListView` ([documentation](https://docs.djangoproject.com/en/1.5/ref/class-based-views/mixins-multiple-object/#multipleobjectmixin), [source code](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/list.py#L11)) and simply gets all objects of a given model ([SC](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/list.py#L29)) issuing `queryset = self.model._default_manager.all()`. The value of `self.model` is what we configured in our class when we wrote `model = Article`.

That's all. Our `ArticleListView` class extracts all `Article` objects from the database, and calls a template passing a context that contains a single variable, `object_list`, instanced with the list of extracted objects.

#### Templates and contexts

Are you satisfied? I'm actually still curious about the template and the context. Let's see what we can find about these topics. First of all, when the class calls `self.render_to_response()` it uses the code that comes from its `TemplateResponseMixin` ancestor ([documentation](https://docs.djangoproject.com/en/1.5/ref/class-based-views/mixins-simple/#templateresponsemixin), [source code](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L81)); the method calls some other functions but its basic behaviour is to create a response using a template and a context. The template, again through a series of calls which you can follow by yourself, comes from `self.template_name` ([SC](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L109)); while `TemplateResponseMixin` defines it `None` ([SC](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L85)), `ListView` does some magic through ancestors ([SC](https://github.com/django/django/blob/stable/1.3.x/django/views/generic/list.py#L128)) to return a template which name derives from the given model. So, in short, our `ArticleListView`, defining an `Article` model, automatically uses a template that is called `article_list.html`.

May we change this behaviour? Of course! This is, after all, the point of using classes instead of functions: easily customize the behaviour. We can change the definition of our class like:

``` python
from django.views.generic.list import ListView
from articles.models import Article

class ArticleListView(ListView):
    model = Article
    template_name = 'sometemplate.html'
```

What does this exactly do? When the `self.render_to_response()` method looks for `self.template_name` this attribute has a value, so there is no need to call the predefined methods and `sometemplate.html` becomes the name of the template used to render the response. This follows a very useful pattern of object-oriented programming; if you are interested I can write something about this topic.

As regards the context, remember that it is only a dictionary of values you want to access when compiling the template. Variable names inside the context (as thus inside the template), data format and data content are completely up to you. When using CBGVs, however, you will find in your context some variables that have been created by the ancestors of your view, as happens for `object_list`. What if you want to show a page with the list of all Articles, but you want to add a value to the context?

Nothing is easier: you just need to override the function that produces the context and change its behaviour. Say, for example, that we want to show the number of total readers of our site, along with the list of articles. Assuming that a `Reader` model is available we can write

``` python
from django.views.generic.list import ListView
from articles.models import Article, Reader

class ArticleListView(ListView):
    model = Article
	
	def get_context_data(self, **kwargs):
		context = super(ArticleListView, self).get_context_data(**kwargs)
		context['readers'] = Reader.objects.count()
		return context
```

As always, when overriding a method we first call the ancestor's one, so that we get the result expected from a normal behaviour of the method, then we add out customizations.

## Conclusion

In this first post I tried to uncover some of the mysteries behind CBVs and CBGVs in Django, by showing step by step what happens to a GET request that hits a class-based view. Hopefully the matter has now been demystified a little!

In the next posts I will discuss DetailView, the generic view to show detail about an object, how to create custom CBVs and how to use CBVs to process forms, i.e. POST requests.

Let me know if this post helped you in understanding the matter and feel free to point out any error or to ask questions.

## Updates

2013-10-29: As pointed out by [mbrochh](http://www.reddit.com/user/mbrochh) on Reddit, there is a very useful resource for Django programmers: [Classy Class-Based Views](http://ccbv.co.uk/). It is a comprensive index of all CBGVs with ancestors and method signatures. Make sure to have it in you Django bookmarks!

2013-10-29: I fixed a couple of typos when overriding `dispatch()`. Thanks to Tom Evans for spotting them.

2013-10-30: Fixed the `__init__()` method of `EvenExtractor`, that was missing the `self` parameter. Thanks [meatypocket](http://www.reddit.com/user/meatypocket).

2015-06-10: [meatypocket](http://www.reddit.com/user/meatypocket) spotted a missing `return` in the `dispatch()` override. Thank you!

## Next articles

* [Digging Up Django Class-based Views - 2](/blog/2013/12/11/digging-up-django-class-based-views-2)

* [Digging Up Django Class-based Views - 3](/blog/2014/02/14/digging-up-django-class-based-views-3)
