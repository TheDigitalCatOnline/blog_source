Title: Digging up Django class-based views - 3
Date: 2014-02-14 16:13:41 +0100
Modified: 2020-03-16 07:00:00 +0000
Category: Programming
Tags: Django, OOP, Python, Python3
Authors: Leonardo Giordani
Slug: digging-up-django-class-based-views-3
Image: digging-up-django-class-based-views
Series: Digging up Django class-based views
Summary: A detailed explanation of the nature of class-based views in Django - detail views, base views, and date-based views

In the first two issues of this short series we discussed the basic concepts of class-based views in Django, and started understanding and using two of the basic generic views Django makes available to you: `ListView` and `DetailView`. Both are views that read some data from the database and show them on a rendered template. We also briefly reviewed the base views that allow us to build heavily customised views, and date-based views.

This third issue will introduce the reader to the class-based version of Django forms. This post is not meant to be a full introduction to the Django form library; rather, I want to show how class-based generic views implement the CUD part of the CRUD operations (Create, Read, Update, Delete), the Read one being implemented by "standard" generic views.

# A very basic example

To start working with CBFs (class-based forms) let's consider a simple example. We have a `StickyNote` class which represents a simple text note with a date:

``` python
class StickyNote(models.Model):
    timestamp = models.DateTimeField()
    text = models.TextField(blank=True, null=True)
```

One of the first things we usually want to do is to build a form that allows the user to create a new entry in the database, in this case a new sticky note. We can create a page that allows us to input data for a new `StickyNote` simply creating the following view

``` python
class NoteAdd(CreateView):
    model = StickyNote
```

It is no surprise that the class is mostly empty. Thanks to inheritance, as happened in the first two posts with standard views, the class contains a bunch of code that lives somewhere in the class hierarchy and works behind the scenes. Our mission is now to uncover that code to figure out how exactly CBFs work and how we can change them to perform what we need.

To make the post easier to follow, please always remember that "class-based form" is a short name for "class-based form view". That is, CBFs are views, so their job is to process incoming HTTP requests and return an HTTP response. Form views do this in a slightly different way than the standard ones, mostly due to the different nature of POST requests compared with GET ones. Let us take a look at this concept before moving on.

# HTTP requests: GET and POST

_Please note that this is a broad subject and that the present section wants only to be a very quick review of the main concepts that are related to Django CBFs_

HTTP requests come in different forms, depending on the **method** they carry. Those methods are called **HTTP verbs** and the two most used ones are **GET** and **POST**. The GET method tells the server that the client wants to retrieve a resource (the one connected with the relative URL) and shall have no side effects (such as changing the resource). The POST method is used to send some data to the server, the given URL being the resource that shall handle the data.

As you can see, the definition of POST is very broad: the server accepts the incoming data and is allowed to perform any type of action with it, such as creating a new entity, editing or deleting one or more of them, and so on.

Keep in mind that forms are not the same thing as POST request. As a matter of fact, they are connected just incidentally: a form is a way to collect data from a user browsing a HTML page, while POST requests are the way that data is transmitted to the server. You do not need to have a form to make a POST request, you just need some data to send. HTML forms are just a useful way to send POST requests, but not the only one.

# Form views

Why are form views different from standard views? The answer can be found looking at the flow of a typical data submission on a Web site:

1. The user browses a web page (GET)
2. The server answers the GET request with a page containing a form
3. The user fills the form and submits it (POST)
4. The server receives and processes data

As you can see the procedure involves a double interaction with the server: the first request GETs the page, the second POSTs the data. So you need to build a view that answers the GET request and a view that answers the POST one.

Since most of the time the URL we use to POST data is the same URL we used to GET the page, we need to build a view that accepts both methods. It is time to dig into the class-based forms that Django provides to understand how they deal with this double interaction.

Let us start with the `CreateView` class we used in our simple example ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L175). It is an almost empty class that inherits from `SingleObjectTemplateResponseMixin` and `BaseCreateView`. The first class deals with the template selected to render the response and we can leave it aside for the moment. The second class ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L160)), on the other hand, is the one we are interested in now, as it implements two methods which names are self explaining, `get` and `post`.

# Processing GET and POST requests

We already met the `get` method in the [previous article]({filename}digging-up-django-class-based-views-2.markdown) when we talked about the `dispatch` method of the `View` class. A quick recap of its purpose: this method is uses to process an incoming HTTP request, and is called when the HTTP method is GET. Unsurprisingly, the `post` method is called when the incoming request is a POST one. The two methods are already defined by an ancestor of the `BaseCreateView` class, namely `ProcessFormView` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L129)), so it is useful to have a look at the source code of this last class:

``` python
class ProcessFormView(View):
    """Render a form on GET and processes it on POST."""
    def get(self, request, *args, **kwargs):
        """Handle GET requests: instantiate a blank version of the form."""
        return self.render_to_response(self.get_context_data())

    def post(self, request, *args, **kwargs):
        """
        Handle POST requests: instantiate a form instance with the passed
        POST variables and then check if it's valid.
        """
        form = self.get_form()
        if form.is_valid():
            return self.form_valid(form)
        else:
            return self.form_invalid(form)
```

As you can see the two methods are pretty straightforward, but it's clear that a lot is going on under the hood.

# The form workflow

Let's start with `get`, which apparently doesn't do much. It just calls `render_to_response` passing the result of `get_context_data`, so we need to track the latter to see what the template will get. `ProcessFormView` or its ancestors don't provide any method called `get_context_data`; instead, the `BaseCreateView` class receives it from `ModelFormMixin`, which in turn receives it from `FormMixin` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L63)).

The class hierarchy is pretty complex, but don't be scared, the important part is that the method `get_context_data` provided by `FormMixin` injects a `'form'` value into the context ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L65)), and the form is provided by the `get_form` method defined in the same class ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L29)), and this eventually uses the `form_class` attribute to instantiate the form ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L27)). As you can see there are plenty of steps, which means plenty of chances to customise the behaviour, if we should need to provide a personalised solution.

It is interesting to have a even more in-depth look at the form creating mechanism, though, as this is the crucial point of the whole GET/POST difference. Once the method `get_form` retrieved the form class, it instantiates it to create the form itself, and the parameters passed to the class are provided by `get_form_kwargs` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L35)). When the HTTP method is GET, `get_form_kwargs` returns a dictionary with the `initial` and `prefix` keys, which are taken from the attributes with the same names. I don't want to dig too much into forms now, as they are out of the scope of the post, but if you read the definition of `BaseForm` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/forms/forms.py#L57)) you will notice that its `__init__` method accepts the same two attributes `inital` and `prefix`. Pay attention that this is a simplification of the whole process, as the `ModelFormMixin` class injects a slightly more complicated version of both `got_form_class` and `get_form_kwargs` to provide naming conventions related to the Django model in use.

Back to `ProcessFormView`, the `post` method does not directly render the template since it has to process incoming data before doing that last step. The method, thus, calls `get_form` directly and then runs the validation process on it, calling then either `form_valid` or `form_invalid`, depending on the result of the test. See the [official documentation](https://docs.djangoproject.com/en/3.0/ref/forms/validation/) for more information about form validation.

This time, `get_form_kwargs` adds two keys to the form when it is instantiated, namely `data` and `files`. These come directly from the `POST` and `FILES` attributes of the request, and contain the data the user is sending to the server.

Last, let's have a look at `form_valid` and `form_invalid`. Both methods are provided by `FormMixin` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L55)), but the former is augmented by `ModelFormMixin` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L123)). The base version of `form_invalid` calls `render_to_response` passing the context data initialised with the form itself. This way it is possible to fill the template with the form values and error messages for the wrong ones, while `form_valid`, in its base form, just returns an `HttpResponseRedirect` to the `success_url`. As I said, `form_valid` is overridden by `ModelFormMixin`, which first saves the form, and then calls the base version of the method.

Let's recap the process until here.

1. The URL dispatcher requests a page containing a form with GET.
2. The `get` method of `ProcessFormView` finds the form class of choice through `get_form_class`
3. The form class is instantiated by `get_form` with the values contained in the `self.initial` dictionary
4. At this point a template is rendered with a context returned by `get_context_data` as usual. The context contains the form.
5. When the use submits the form the URL dispatcher requests the page with a POST that contains the data
6. The `post` method of `ProcessFormView` validates the form and acts accordingly, rendering the page again if the data is invalid or processing it and rendering a success template with the newly created object.

# Update and Delete operations

This rather rich code tour unveiled the inner mechanism of the `CreateView` class, which can be used to create a new object in the database. The `UpdateView` and `DeleteView` classes follow a similar path, with minor changes to perform the different action they are implementing.

`UpdateView` wants to show the form already filled with values, so it instantiates an object before processing the request ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L189)). This makes the object available in the keywords dictionary under the `instance` key ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L107)), which is used by model forms to initialize the data ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/forms/models.py#L293)). The `save` method of `BaseModelForm` is smart enough to understand if the object has been created or just changed ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/forms/models.py#L454) so the `post` method of `UpdateView` works just like the one of `CreateView`.

`DeleteView` is a bit different from `CreateView` and `UpdateView`. As [the official documentation](https://docs.djangoproject.com/en/3.0/ref/class-based-views/generic-editing/#deleteview) states, if called with a GET method it shows a confirmation page that POSTs to the same URL. So, as for the GET requests, `DeleteView` just uses the `get` method defined by its ancestor `BaseDetailView` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/detail.py#L105)), which renders the template putting the object in the context. When called with a POST request, the view uses the `post` method defined by `DeletionMixin` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L217), which just calls the `delete` method of the same class ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L206)). This performs the deletion on the database and redirects to the success URL.

# Final words

As you can see, the structure behind the current implementation of Django class-based form views is rather complex. This allows the user to achieve complex behaviours like the CUD operations just by defining a couple of classes as I did in the simple example at the beginning of the post. Most of the time, however, such a simplification makes it difficult for the programmer to understand how to achieve the desired changes to the class behaviour. So, the purpose of this big tour I made inside the Django source code was to give an insight of what methods are called in the life cycle of your HTTP request so that you can better identify what methods you need to override.

When performing special actions that fall outside the standard CUD operations you better inherit from `FormView` ([CODE](https://github.com/django/django/blob/stable/3.0.x/django/views/generic/edit.py#L156)). The first thing to do is to check if and how you need to customize the `get` and `post` methods; remember that you either need to implement the full behaviour of those methods or make you changes and call the parent implementation. If this is not enough for your application consider overriding one of the more dedicated methods, such as `get_form_kwargs` or `form_valid`.

This post ends the series "Digging Up Django Class-based Views". Stay tuned for other [articles on Django](/categories/django/)!

# Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.
