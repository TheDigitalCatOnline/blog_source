Title: Digging up Django class-based views - 2
Date: 2013-12-11 09:00 +0200
Category: Programming
Tags: Python, Django
Authors: Leonardo Giordani
Slug: digging-up-django-class-based-views-2
Series: "Digging up Django class-based views"
Summary:

## Abstract

_This post refers to Django 1.5. Please be warned that some of the matters discussed here, some solutions or the given code can be outdated by more recent Django versions_

In the first installment of this short series, I introduced the theory behind Django class-based views and the reason why in this context classes are more powerful than pure functions. I also introduced one of the generic views Django provides out-of-the-box, which is `ListView`.

In this second post I want to talk about the second most used generic view, `DetailView`, and about custom querysets and arguments. Last, I'm going to introduce unspecialized class-based views that allow you to build more complex Web pages. To fully understand `DetailView`, however, you need to grasp two essential concepts, namely **querysets** and **view parameters**. So I'm sorry for the learn-by-doing readers, but this time too I'm going to start with some pure programming topics.

## QuerySets or the art of extracting information

One of the most important parts of Django is the ORM (Object Relational Mapper), which allows you to access the underlying database just like a collection of Python objects. As you know, Django provides you tools to simplify the construction of DB queries; they are **managers** (the `.objects` attribute of any models, for example) and **query methods** (`get()`, `filter()`, and friends). Pay attention because things here are slightly more magical than you can think at a first glance.

When you use one of the methods of a manager you get as a result a `QuerySet`, which most of the time is used as a list, but is more than this. You can find [here](https://docs.djangoproject.com/en/1.5/topics/db/queries/) and [here](https://docs.djangoproject.com/en/1.5/ref/models/querysets/) the official documentation about queries and QuerySets, a very recommended reading.

What I want to stress here is that QuesySets are not evaluated until you perform an action that access the content like slicing or iterating on it. This means that we can build QuerySets, pass them to functions, store them, and even build them programmatically or metaprogramming them without the DB being hit. If you think at QuerySets as recipes you are not far from the truth: they are objects that store how you want to retrieve the data of your interest. Actually retriving them is another part of the game. This separation between the definition of something and its execution is called **lazy evaluation**.

Let me give you a very trivial example to show why the lazy evaluation of QuerySets is important.

``` python
[...]

def get_oldest_three(queryset):
	return queryset.order_by['id'][0:2]

old_books = get_oldest_three(Book.objects.all())
old_hardcover_books = \
    get_oldest_three(Book.objects.filter('type=Book.HARDCOVER'))
```

As you can see the `get_oldest_three()` method is just filtering an incoming QuerySet (which can be of any type); it simply orders the objects and gets the first three inserted in the DB. The important thing here is that we are using QuerySets like pure 'algorithms', or descriptions of a procedure. When creating the `old_books` variable we are just telling the `get_oldest_three()` method "Hey, this is the way I extract the data I'm interested in. May you please refine it and return the actual data?"

Being such flexible objects, QuerySets are an important part of generic views, so keep them warm for the upcoming banquet.

## Being flexible: parametric views

URLs are the API of our Web site or service. This can be more or less evident for the user that browses through the pages, but from the programmer's point of view URLs are the entry points of a computer system. As such, they are not very different from the API of a library: here, static pages are just like constants, or functions that always return that same value (such as a configuration parameter), while dynamic pages are like functions that process incoming data (parameters) and return a result.

So Web URLs can accept parameters, and our underlying view shall do the same. You basically have three methods to convey parameters from the user's browser to your server using HTTP. The first method is named [query string](http://en.wikipedia.org/wiki/Query_string) and lists parameters directly in the URL through a universal syntax. The second method is storing parameters in the HTTP request body, which is what POST requests do. We will discuss this method in a later post about forms.

The first method has one big drawback: most of the time URLs are long (and sometimes *too* long), and difficult to use as a real API. To soften this effect the concept of [clean URL](http://en.wikipedia.org/wiki/Clean_URL) arose, and this is the way Django follows natively (though, if you want, you can also stick to the query string method).

Now, [you know](https://docs.djangoproject.com/en/1.5/topics/http/urls/) that you can collect parameters contained in the URL parsing it with a regular expression; what we need to discover is how class-based views receive and process them.

In the previous post we already discussed the `as_view()` method that shall instance the class and return the result of `dispatch()` ([views/generic/base.py#L46](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L46)).

``` python
    @classonlymethod
    def as_view(cls, **initkwargs):
        """
        Main entry point for a request-response process.
        """
        # sanitize keyword arguments
        for key in initkwargs:
            if key in cls.http_method_names:
                raise TypeError("You tried to pass in the %s method name as a "
                                "keyword argument to %s(). Don't do that."
                                % (key, cls.__name__))
            if not hasattr(cls, key):
                raise TypeError("%s() received an invalid keyword %r. as_view "
                                "only accepts arguments that are already "
                                "attributes of the class." % (cls.__name__, key))

        def view(request, *args, **kwargs):
            self = cls(**initkwargs)
            if hasattr(self, 'get') and not hasattr(self, 'head'):
                self.head = self.get
            self.request = request
            self.args = args
            self.kwargs = kwargs
            return self.dispatch(request, *args, **kwargs)

        # take name and docstring from class
        update_wrapper(view, cls, updated=())

        # and possible attributes set by decorators
        # like csrf_exempt from dispatch
        update_wrapper(view, cls.dispatch, assigned=())
        return view
```

Now look at what the `view()` wrapper function actually does with the instanced class (here line 21, [views/generic/base.py#L65](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L65)); not surprisingly it takes the `request`, `args` and `kwargs` passed by the URLconf passes and converts them into as many class attributes with the same names.

This means that *everywhere in our CBVs* we can access the original call parameters simply reading `self.request`, `self.args` and `self.kwargs`, where `*args` and `**kwargs` are the unnamed and named values extracted by the URLconf regular expression.

## Details

Just after listing things, one of the most useful things a Web site does is giving details about objects. Obviously any e-commerce site is made for the most part by pages that list products and show product details, but also a blog is made of one or more pages with a list of posts and a page for each of them. So building a detailed view of the content of our database is worth learning.

To help us in this task Django provides `DetailView`, which indeed deals, as the name suggests, with the details of what we get from the DB. While `ListView`'s basic behaviour is to extract the list of all objects with a given model, `DetailView` extracts a single object. How does it know what object shall be extracted?

When `dispatch()` is called on an incoming HTTP request the only thing it does is to look at the `method` attribute, which for `HttpRequest` objects contains the name of the HTTP verb used (e.g. `'GET'`); then `dispatch()` looks for a method of the class with the lowercase name of the verb (e.g. `'GET'` --> `get()`) ([views/generic/base.py#L78](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L78)). This handler is then called with the same parameters of `dispatch()`, namely the `request` itself, `*args` and `**kwargs`.

`DetailView` has no body and inherits everything from two classes; the second one, `BaseDetailView`, implements the `get()` method ([views/generic/detail.py#L107](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/detail.py#L107)).

``` python
def get(self, request, *args, **kwargs):
    self.object = self.get_object()
    context = self.get_context_data(object=self.object)
    return self.render_to_response(context)
```

As you can see this method extracts the single object it shall represent calling `self.get_object()`, then calls `self.get_context_data()` (that we met in the previous post) and last the familiar `self.render_to_response()` that is the class equivalent of the well know Django function. The method `self.get_object()` is provided by `BaseDetailView`'s ancestor `SingleObjectMixin` ([generic/detail.py#L10](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/detail.py#L10)): the most important parts of its code, for the sake of our present topic are

``` python
def get_object(self, queryset=None):
    if queryset is None:
        queryset = self.get_queryset()

    pk = self.kwargs.get(self.pk_url_kwarg, None)
    if pk is not None:
        queryset = queryset.filter(pk=pk)
    
    obj = queryset.get()
    
    return obj
```

**Warning**: I removed many lines from the previous function to improve readability; please check the original source code for the complete implementation.

The code shows where `DetailView` gets the queryset from; the `get_queryset()` method is provided by `SingleObjectMixin` itself and basically returns `self.queryset` if present, otherwise returns all objects of the given model (acting just like `ListView` does). This `queryset` is then refined by a `filter()` and last by a `get()`. Here `get()` is not used directly (I think) to manage the different error cases and raise the correct exceptions.

The parameter `pk` used in `filter()` comes directly from `self.kwargs`, so it is taken directly from the URL. Since this is a core concept of views in general I want to look at this part carefully.

Our `DetailView` is called by an URLconf that provides a regular expression to parse the URL, for example `url(r'^(?P<pk>\d+)/$',`. This regex extracts a parameter and gives it the name `pk`, so `kwargs` of the view will contain `pk` as key and the actual number in the URL as value. For example the URL `123/` will result in `{'pk': 123}`. The default behaviour of `DetailView` is to look for a `pk` key and use it to perform the filtering of the queryset, since `self.pk_url_kwarg` is `'pk'`.

So if we want to change the name of the parameter we can simply define the `pk_url_kwarg` of our class and provide a regex that extract the primary key with the new name. For example `url(r'^(?P<key>\d+)/$',` extracts it with the name `key`, so we will define `pk_url_kwarg = 'key'` in our class.

From this quick exploration we learned that a class inheriting from `DetailView`:

* provides a context with the `object` key initialized to a single object
* **must** be configured with a `model` class attribute, to know what objects to extract
* **can** be configured with a `queryset` class attribute, to refine the set of objects where the single object is extracted from
* **must** be called from a URL that includes a regexp that extracts the primary key of the searched object as `pk`
* **can** be configured to use a different name for the primary key through the `pk_url_kwarg` class attribute

The basic use of `DetailView` is thus exemplified by the following code.

``` python
class BookDetail(DetailView):
    model = Book
    
urlpatterns = patterns('',
    url(r'^(?P<pk>\d+)/$',
        BookDetail.as_view(),
        name='detail'),
    )
```

The view extracts a single object with the `Book` model; the regex is configured with the standard `pk` name.

As shown for `ListView` in the previous post, any CBV uses `get_context_data()` to return the context dictionary to the rendering engine. So views that inherit from `DetailView` can add data to the context following the same pattern

``` python
class BookDetail(DetailView):
    model = Book

    def get_context_data(self, **kwargs):
		context = super(BookDetail, self).get_context_data(**kwargs)
		context['similar'] = get_similar_books(self.object)
		return context

urlpatterns = patterns('',
    url(r'^(?P<pk>\d+)/$',
        BookDetail.as_view(),
        name='detail'),
    )
```

As explained before, you can access the object being shown through `self.object`, which in the above example is passed to a service function we implemented somewhere in our code.

## Using the base views

Sometimes, when dealing with complex pages, the generic display CBVs that Django provides are not the right choice. This usually becomes evident when you start overriding method to prevent the view to perform its standard behaviour. As an instance say that you want to show detailed information of more than one object: probably DetailView will soon show its limits, being built to show only one object.

In all those cases that cannot be easily solved by one of the generic display CBVs, your way goes through other classes: `RedirectView`, `TemplateView`, and `View`. The documentation for these base views is [here](https://docs.djangoproject.com/en/1.5/ref/class-based-views/base/), while the source code is in [views/generic/base.py](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py).

I'm not going to fully describe those views; I want however to briefly point out some peculiarities.

`View` is by now an old friend of us; we met it when we discussed the `as_view()` and `dispatch()` method. It is the most generic view class and can be leveraged to perform very specialized tasks such as rendering pages without templates (for example when returning JSON data in AJAX techniques).

`TemplateView` is the best choice to render pages from a template keeping in the meanwhile freedom as regards the context content. Chances are that this is going to be the view you will use the most after `ListView` and `DetailView`. Basically you just need to inherit from it and define the `get_context_data()` method. As you can see from the [source code](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L147) `TemplateView` answers to GET requests only.

`RedirectView`, as the name implies, is used to redirect a request. The redirection mechanism is very simple: its `get()` method returns a `HttpResponseRedirect` to the URL defined by the `url` class attribute. The class exhibits a very interesting behaviour ([views/generic/base.py#L195](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/base.py#L195)) when called through HTTP methods other than GET (namely HEAD, POST, OPTIONS, DELETE, and PUT): it "converts" the method to GET simply calling `get()` from the respective method (`head()`, `post()`, and so on). In the next post I'll show how to leverage this simple technique to show the user a prefilled form.

## Date-based views

Django provides other class-based views that simplify dealing with objects extracted or ordered by date. As a programmer, you know that dealing with dates is sometimes at least awkward; the views in [views/generic/dates.py](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/dates.py) aims to help you to tame your date-based objects; any object that contains a date (e.g. post date for articles, birth date for people, log date for messages, etc) can be processed by these views. You can find the official documentation [here](https://docs.djangoproject.com/en/1.5/ref/class-based-views/generic-date-based/).

Remember that date-based views are CBVs, so they are based on `View`, just like `ListView` or `TemplateView`. So, apart from their specialization on date processing, they behave the same (`get_context_data()`, `get()`, `dispatch()`, and so on).

## Conclusion

In this post we covered `DetailView` in deep and, more quickly, all the remaining base and data-based views. In the next post we will step into the rich (and strange) world of forms.

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post or to ask for an in-depth analysis of some topic. [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) is the best place to submit corrections.

## Previous articles

* [Digging Up Django Class-based Views - 1](/blog/2013/10/28/digging-up-django-class-based-views-1)

## Next articles

* [Digging Up Django Class-based Views - 3](/blog/2014/02/14/digging-up-django-class-based-views-3)
