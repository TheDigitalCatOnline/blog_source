Title: Digging up Django class-based views - 1
Date: 2013-10-28 08:43 +0200
Modified: 2020-03-16 07:00:00 +0000
Category: Programming
Tags: Django, OOP, Python, Python3
Authors: Leonardo Giordani
Slug: digging-up-django-class-based-views-1
Image: digging-up-django-class-based-views
Series: Digging up Django class-based views
Summary: A detailed explanation of the nature of class-based views in Django - list views and GET requests

Django 3 was released at the end of 2019, so I think it is high time I revisited my successful [series of post]({filename}digging-up-django-class-based-views-1.markdown) about class-based views in Django. Those posts date back to 2013 and have been written with Django 1.5 in mind, and with examples from that code base. Now Django is already two versions older, but class-based views are still a big part of the framework, so I believe it makes sense to refresh the content of those posts. Moreover, I didn't have a chance to study Django 3 yet, so as per tradition of this blog, I will make my personal investigation available to everyone.

If you are a novice Python programmer, and just approached Django to start your career in web development, chances are that you were puzzled by many things, and class-based views (CBVs) are definitely among those. CBVs are apparently very easy to use, in the simple cases, but it might not be clear how to extend them to match more complicated use cases, as the development of a project proceeds. The official documentation is very good, but to master CBVs you need to understand object-oriented concepts like classes (well, obviously), delegation, and method overriding.

If you need to brush up on these concepts you might find useful to read the following posts here on the blog:

* [Object-Oriented Programming in Python 3]({filename}python-3-oop-part-1-objects-and-types.markdown)
* [Method overriding in Python]({filename}method-overriding-in-python.markdown)

# What are CBVs?

Class-based views are, Django views based on Python classes. This means that, to master them, you need to understand both Django views and Python classes, so let's give a quick definition of them.

A Django view is a piece of code that processes an incoming HTTP request and returns an HTTP response, nothing more, nothing less. A Python class is the implementation of the Object-Oriented concept of class in the Python language.

So, a view needs to be a [callable](https://docs.python.org/3/library/functions.html#callable), and this includes functions and classes. Thus, to understand the advantages of class-based views over function-based views we shall discuss the merits of classes over functions. The latter sentence could be the title of a 10 volumes book on programming (followed by another 10 volumes book titled "Merits of functions over classes"), so I am just going to scratch the surface of the matter here. If you want to dig more into the subject, please read the series on Python 3 OOP that I linked above, where you will find all the gory details that you are craving for.

# Starting off with Python classes

The main point classes is to implement encapsulation: they represent a way of coupling data and functions. Doing this, a class loses the dynamic essence of a procedure, which exists only while it is running, and becomes a living entity, something that sits there, caring for its data, and reacts when we call its functions (methods).

A good analogy for a class is a finite-state machine: once the class has been initialized, methods are what we use to make the machine move between states. If we do not call methods, the class simply waits there without complaining.

As an example, let's look at a very simple procedure that extracts the even numbers from an iterable like a list

``` python
def extract_even_numbers(alist):
    return [i for i in alist if i%2 == 0]
```

The example is very trivial, but, as code naturally tends to become more complicated, it's better to start with simple examples. A class version of this function could be written as

``` python
class EvenExtractor:
    def __init__(self, alist):
        self.l = alist
        
    def extract(self):
        return [i for i in self.l if i%2 == 0]
```

The two are very similar, and it might look like we haven't changed anything. Indeed, the difference is subtle but remarkable. Now the `EvenExtractor` class has two parts, the first being the initialization and the second being the actual extraction, and we can have the class in one of three states: before initialization (`EvenExtractor`), after initialization (`e = EvenExtractor([1,4,5,7,12])`), and after extraction (`l = e.extract()`).

Converting the procedure to a class, then, we obtained a rich tool that can execute its job step by step and, in general, can work in a non linear way, as we might add further methods, and thus more states.

# Delegation is the key

The real power of classes used as finite-state machines lies in the concept of delegation. This is a mechanism through which a class can delegate some work to another class, avoiding to duplicate code, and thus favouring code reuse and generalisation.

(You might notice that I don't mention inheritance, but delegation, which is implemented by both composition and inheritance. I am a strong supporter of an OO design principle that states "Favour composition over inheritance". I keep reading too many introductions to object-oriented that stress too much the inheritance mechanism and leave composition aside, raising a generation of OOP programmers that, instead of building systems populated by many small collaborating objects, create nightmares infested by giant all-purpose things that sometimes resemble more an operating system than a system component.)

Let's continue the above example, improving the `__init__` method of the `EvenExtractor` class:

``` python
class EvenExtractor:
    def __init__(self, alist):
        self.l = [int(elem) for elem in alist]
        
    def extract(self):
        return [i for i in self.l if i%2 == 0]
```

Now the class performs an important action in its initialization phase, converting all elements of the input to integers. Some days after this change, however, we might realise that we could also profitably use a class that extracts odd elements from a list. Being responsible object oriented programmers we write

```  python
class OddExtractor(EvenExtractor):
    def extract(self):
        return [i for i in self.l if i%2 != 0]
```

and call it a day. Through the inheritance mechanism expressed by that `(EvenExtractor)` signature of the new class, we first defined something that is exactly the same thing as `EvenExtractor`, with the same methods and attributes, but with a different name. Then we changed the behaviour of the new class but only for the extraction part by overriding the method.

To summarise the lesson: using classes and delegation you can build finite-state machines that are easily customizable to suit your exact needs. This obviously is just one of the many points of view under which you can consider classes, but it is the one we need to understand Django CBVs.

# Back to Django

Let's start discussing a practical use of what we learned so far, reviewing how Django uses Python classes and delegation to provide views.

A Django view is a perfect example of a finite-state machine. It takes an incoming request and makes it flow through different processing steps until a final response is produced, which is then sent back to the user. CBVs are a way for the programmer to write their views leveraging the object-oriented paradigm. In this context Class-based Generic Views are the "batteries included" of Django views, the building blocks that the framework provides out of the box.

Let's dig into one of the examples of the official Django docs; [here](https://docs.djangoproject.com/en/3.0/ref/class-based-views/generic-display/#django.views.generic.list.ListView) you find the API of the beloved `ListView`, a generic view to deal with a list of things (extracted from the database). I slightly simplified the example provided by the documentation to avoid having too much on our plate.

``` python
from django.views.generic.list import ListView

from articles.models import Article

class ArticleListView(ListView):

    model = Article
```

This example assumes that `articles` is your application and `Article` is one of its models.

You can see here the full power of inheritance. We just derived `ArticleListView` from `ListView`, and changed the `model` class attribute. How can this work? How can this class process incoming requests and what are the outputs? The official documentation states "While this view is executing, `object_list` will contain the list of objects (usually, but not necessarily a queryset) that the view is operating upon."; this leaves many dark corners, however, and if you are a novice, chances are that you are already lost.

Since `ArticleListView` derives from `ListView`, the latter is the class we have to analyse to understand how incoming data is processed. To do this you need to look at the [documentation](https://docs.djangoproject.com/en/3.0/ref/class-based-views/generic-display/#listview), and if something is still unclear you can freely look at the [source code](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/list.py). In the following paragraphs I will summarise what happens when Django calls the sample `ArticleListView` class shown above, and you will find links called "DOCS" for the official documentation, and "CODE" for the relevant source code, if you want to read it by yourself.

# URL dispatchers and views

A CBV cannot directly be used in your URL dispatcher; instead you have to give the result of the `as_view` method ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/base.py#L49)), which defines a function that instances the class ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/base.py#L61)) and calls the `dispatch` method ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/base.py#L71)); then the function is returned ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/base.py#L81)) to be used in the URL dispatcher. As a user, we are interested only in the fact that the _entry point_ of the class (the method called when a request hits the URL linked with it) is `dispatch`.

Let's use this knowledge to print out a string on the console each time a request is served by our CBV. I will run through this simple task step by step, since it shows exactly how you have to deal with CBVs when solving real problems.

If we define the `ArticleListView` class this way

``` python
from django.views.generic.list import ListView

from articles.models import Article

class ArticleListView(ListView):

    model = Article

	def dispatch(self, request, *args, **kwargs):
		return super().dispatch(request, *args, **kwargs)
```

the class does not change its behaviour. What we did was to override the `dispatch` method with a call to the parent's method, i.e. we explicitly wrote what Python does by default. You can find detailed information about `super` in the [official documentation](https://docs.python.org/3/library/functions.html#super) and in [this post]({filename}python-3-oop-part-3-delegation-composition-and-inheritance.markdown) on the blog. Please be sure you understand the star and double star notation to define variable number of arguments; the official documentation is [here](https://docs.python.org/3.8/tutorial/controlflow.html#positional-or-keyword-arguments).

Since views are automatically called by the framework, the latter expects them to comply with a very specific API, so when overriding a method you have to provide the same signature of the original one. The signature of `dispatch` can be found [here](https://docs.djangoproject.com/en/3.0/ref/class-based-views/base/#django.views.generic.base.View.dispatch).

The `dispatch` method receives a `request` argument, which type is `HttpRequest` ([documentation](https://docs.djangoproject.com/en/3.0/ref/request-response/#httprequest-objects)), so we can print it on the console with the standard `print` function

``` python
from django.views.generic.list import ListView

from articles.models import Article

class ArticleListView(ListView):

    model = Article

	def dispatch(self, request, *args, **kwargs):
		print(request)
		return super().dispatch(request, *args, **kwargs)
```

This prints the content of the `request` object on the standard output of the server that is running the Django project. If you are running the Django development server, you will find the output on the text console where you issued the command `python manage.py runserver`.

This, in a nutshell, is the standard way of dealing with Django CBGVs: inherit from a predefined class, identify which methods you need to change, override them complying with their signature and calling the parent's code somewhere in the new code.

The full list of methods `ListView` uses when processing incoming requests is listed on its [official documentation page](https://docs.djangoproject.com/en/3.0/ref/class-based-views/generic-display/#listview) in the "Method Flowchart" section; in the "Ancestors (MRO)" section you can see that `ListView` inherits from a good number of other classes. MRO stands for Method Resolution Order and has to deal with multiple inheritance: if you are eager to deal with one of the most intricate Python topics feel free to read [this](https://docs.python.org/3.8/tutorial/classes.html#multiple-inheritance).

# Incoming GET requests

Back to our `ArticleListView`. The `dispatch` method of the parent reads the `method` attribute of the `request` object and selects a handler to process the request itself ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/base.py#L93)): this means that if `request.method` is `'GET'`, which is the HTTP way to say that we are _reading_ a resource, `dispatch` will call the `get` method of the class.

The `get` method of `ListView` comes from its `BaseListView` ancestor ([DOCS](https://docs.djangoproject.com/en/3.0/ref/class-based-views/generic-display/#django.views.generic.list.BaseListView), [CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/list.py#L141)). As you can see, the function basically initializes the attribute `object_list` with the result of the call `get_queryset()`, creates a context calling the method `get_context_data` and calls `render_to_response`.

Are you still with me? Don't give up, we are almost done, at least with ListView. The method `get_queryset` comes from the `MultipleObjectMixin` ancestor of `ListView` ([DOCS](https://docs.djangoproject.com/en/3.0/ref/class-based-views/mixins-multiple-object/#multipleobjectmixin), [CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/list.py#L9)) and simply gets all objects of a given model ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/list.py#L33)) running `queryset = self.model._default_manager.all()`. The value of `model` is what we configured in our class when we wrote `model = Article`. I hope at this point something start to make sense in your head.

That's all, actually. Our `ArticleListView` class extracts all `Article` objects from the database, and calls a template passing a context that contains a single variable, `object_list`, instanced with the list of extracted objects.

# Templates and contexts

Are you satisfied? I'm actually still curious about the template and the context. Let's see what we can find about these topics. First of all, when the class calls `render_to_response` it uses the code that comes from its `TemplateResponseMixin` ancestor ([DOCS](https://docs.djangoproject.com/en/3.0/ref/class-based-views/mixins-simple/#templateresponsemixin), [CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/base.py#L117)); the method initialises the class `TemplateResponse` passing a template and a context. The template, through a series of calls which you can follow by yourself, comes from `template_name` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/base.py#L150)); while `TemplateResponseMixin` initializes it as `None` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/base.py#L119)), `ListView` performs some magic tricks through ancestors ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/list.py#L165)) to return a template which name derives from the given model. In short, our `ArticleListView`, defining an `Article` model, automatically uses a template that is called `article_list.html`.

May we change this behaviour? Of course! This is, after all, the point of using classes instead of functions: easily customisable behaviour. We can change the definition of our class to be

``` python
from django.views.generic.list import ListView

from articles.models import Article

class ArticleListView(ListView):

    model = Article
    template_name = 'sometemplate.html'
```

Let's review what this does step by step. When the response is created, Django runs the code of `render_to_response` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/base.py#L124)), which in turn calls `get_template_names`. Pay attention that this method returns a list of names, as Django will use the first available among them, scanning them in order. This method is overridden in `ListView` by its superclass `MultipleObjectTemplateResponseMixin` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/list.py#L165)). This calls the same method of its own superclass `TemplateResponseMixin` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/base.py#L140)), which returns the attribute we set in the `ArticleListView` class ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/base.py#L150)). The mixing goes on and appends to the list the template file name derived from the model ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/list.py#L181)) and finally returns the list, which at this point is `['sometemplate.html', 'article_list.html']`.

As for the context, remember that it is only a dictionary of values you want to be able to access when compiling the template. Variable names inside the context, data format, and data content are completely up to you. When using CBGVs, however, you will find in your context some variables that have been created by the ancestors of your view, as happens for `object_list`. What if you want to show a page with the list of all articles, but you want to add a value to the context?

Easy task: you just need to override the function that produces the context and change its behaviour. Say, for example, that we want to show the number of total readers of our site, along with the list of articles. Assuming that a `Reader` model is available we can write

``` python
from django.views.generic.list import ListView

from articles.models import Article, Reader

class ArticleListView(ListView):
    model = Article
	
	def get_context_data(self, **kwargs):
		context = super().get_context_data(**kwargs)
		context['readers'] = Reader.objects.count()
		return context
```

As always, when overriding a method we need to ask ourselves if we need to call the original method. In this case, we want to merely augment the content of the context and not replace it, so we call `super().get_context_data(**kwargs)` first, and we add the value that we need to that. pay attention that this might not be always the case, as it depends on the logic of your override.

# Final words

In this first post I tried to uncover some of the mysteries behind CBVs and CBGVs in Django, showing exactly what happens to a GET request that hits a class-based view. Hopefully the matter has now been demystified a little! In the next posts I will discuss `DetailView`, the generic view to show detail about an object, how to create custom CBVs, and how to use CBVs to process forms, i.e. accept POST requests.

# Updates

2020-03-12: A global review of the post, which has been updated with the latest Django code (3.0)

2013-10-29: As pointed out by [mbrochh](https://www.reddit.com/user/mbrochh) on Reddit, there is a very useful resource for Django programmers: [Classy Class-Based Views](http://ccbv.co.uk/). It is a comprensive index of all CBGVs with ancestors and method signatures. Make sure to have it in you Django bookmarks!

2013-10-29: I fixed a couple of typos when overriding `dispatch()`. Thanks to Tom Evans for spotting them.

2013-10-30: Fixed the `__init__()` method of `EvenExtractor`, that was missing the `self` parameter. Thanks [meatypocket](https://www.reddit.com/user/meatypocket).

2015-06-10: [meatypocket](https://www.reddit.com/user/meatypocket) spotted a missing `return` in the `dispatch()` override. Thank you!

# Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.
