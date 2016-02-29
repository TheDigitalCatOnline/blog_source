Title: Digging up Django class-based views - 3
Date: 2014-02-14 16:13:41 +0100
Category: Programming
Tags: Python, Django
Authors: Leonardo Giordani
Slug: digging-up-django-class-based-views-3
Series: "Digging up Django class-based views"
Summary:

_This post refers to Django 1.5. Please be warned that some of the matters discussed here, some solutions or the given code can be outdated by more recent Django versions_

In the first two issues of this short series we wandered around the basic concepts of class-based views in Django, and started understanding and using two of the basic generic views Django makes available to you: `ListView` and `DetailView`. Both are views that read some data from the database and show them on a rendered template.

This third issue wants to introduce the reader to the class-based version of Django forms. This post is not meant to be a full introduction to the Django form library; rather, I want to show how class-based generic views implement the CUD part of the CRUD operations (Create, Read, Update, Delete), the Read one being implemented by "standard" generic views.

## A very basic example

To start working with CBFs (class-based forms) let's say we are working with a `StickyNote` class which represents a simple text note with a date:

``` python
class StickyNote(models.Model):
    timestamp = models.DateTimeField()
    text = models.TextField(blank=True, null=True)
```

One of the first things we usually want to do is to build a form that allows the user to create a new entry in the database, in this case a new sticky note. The functional version of such a form would be the following:

``` python
def note_add(request):
    form = StickyNoteForm(request.POST or None)
    if form.is_valid():
        new_note = form.save()
        new_note.save()
    return render_to_response('note_add.html')
    
urlpatterns = patterns('',
    url(r'^note_add/$', 'note_add'),
)
```

which is not too complex to grasp. Note that I left aside some imports; the `StickNoteForm` class is built using a [model form](https://docs.djangoproject.com/en/1.5/topics/forms/modelforms/). Since you [already know](https://docs.djangoproject.com/en/1.5/topics/forms/) how functional form views work, let's compare it with the same view expressed with a class:

``` python
class NoteAdd(CreateView):
    model = StickyNote
```

It is no surprise that most of the code went away, thanks to inheritance. As happened in the first two posts with standard views, the class mechanism provides us with a bunch of code that lives somewhere in the class hierarchy and works behind the scenes. Our mission is now to uncover that code to figure out how exactly CBFs work and how we can change them to perform what we need.

To make the post easier to follow, please always remember that "class-based form" is a short name for "class-based form view". That is, CBFs are views, so their job is to process incoming HTTP requests and return a HTTP response. Form views do this in a slightly different way than the standard ones, mostly due to the different nature of POST requests compared with GET ones. Let us take a look at this concept before moving on.

## HTTP requests: GET and POST

_Please note that this is a broad subject and that the present section wants only to be a very quick review of the main concepts that are related to Django CBFs_

HTTP requests come in different forms, depending on the **method** they carry. Those methods are called **HTTP verbs** and the two most used ones are **GET** and **POST**. The GET method tells the server that the client wants to retrieve a resource (the one connected with the relative URL) and shall have no side effects (such as changing the resource). The POST method is used to send some data to the server, the given URL being the _resource_ that shall handle the data.

As you can see, the definition of POST is very broad: the server accepts the incoming data and is allowed to perform any type of action with it, such as creating a new entity, editing or deleting one or more of them, and so on.

Keep in mind that forms are not the same thing as POST request. As a matter of fact, they are connected just incidentally: a form is a way to collect data from a user browsing a HTML page while POST is the way that data is transmitted to the server. You do not need to have a form to make a POST request, you just need some data to send. HTML forms are just a useful way to send POST requests, but not the only one.

## Form views

Why are form views different from standard views? The answer can be found looking at the flow of a typical data submission on a Web site:

1. The user browses a web page (GET)
2. The server answers the GET request with a page containing a form
3. The user fills the form and submits it (POST)
4. The server receives and processes data

As you can see the procedure involves a double interaction with the server: the first request GETs the page, the second POSTs the data. So you need to build a view that answers the GET request and a view that answers the POST one.

Since most of the time the URL we use to POST data is the same URL we used to GET the page, we need to build a view that accepts both methods. This is the main reason of the pattern you are used to see in functional form views in Django. [The official Django documentation](https://docs.djangoproject.com/en/1.5/topics/forms/) on the subject uses this snippet of code

``` python
def contact(request):
    if request.method == 'POST': # If the form has been submitted...
        form = ContactForm(request.POST) # A form bound to the POST data
        if form.is_valid(): # All validation rules pass
            # Process the data in form.cleaned_data
            # ...
        return HttpResponseRedirect('/thanks/') # Redirect after POST
    else:
        form = ContactForm() # An unbound form

    return render(request, 'contact.html', {
        'form': form,
    })
```

As you can see the first conditional path deals with the data submission (POST) while the else part deals with the usual case of a GET request.

Now it is time to dig into the class-based forms that Django provides us to understand how they deal with this double interaction.

Let us start with the `CreateView` class we used in our simple example, which is defined in [views/generic/edit.py#202](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L202). It is an almost empty class that inherits from `SingleObjectTemplateResponseMixin` and from `BaseCreateView`. The first class deals with the template selected to render the response and we can leave it aside for the moment. The second class, on the other hand, can be found a couple of lines above, at [views/generic/edit.py#L187](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L187), and implements two methods which names are self explaining, `get()` and `post()`.

#### Processing GET and POST requests

We already met the `get()` method in the [past article](/blog/2013/12/11/digging-up-django-class-based-views-2) when we talked about the `dispatch()` method of the `View` class. A quick recap of its purpose: this method is called when the incoming HTTP request carries the GET verb and is used to process the request itself. Not surprisingly, the `post()` method is called when the incoming request is a POST one. The two methods are already defined by an ancestor of the `BaseCreateView` class, namely `ProcessFormView` ([views/generic/edit.py#L145](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L145)). It is useful to take a peek at the source code of this last class:

``` python
class ProcessFormView(View):
    """
    A mixin that renders a form on GET and processes it on POST.
    """
    def get(self, request, *args, **kwargs):
        """
        Handles GET requests and instantiates a blank version of the form.
        """
        form_class = self.get_form_class()
        form = self.get_form(form_class)
        return self.render_to_response(self.get_context_data(form=form))

    def post(self, request, *args, **kwargs):
        """
        Handles POST requests, instantiating a form instance with the passed
        POST variables and then checked for validity.
        """
        form_class = self.get_form_class()
        form = self.get_form(form_class)
        if form.is_valid():
            return self.form_valid(form)
        else:
            return self.form_invalid(form)

```

As you can see the two methods are pretty straightforward. They both retrieve the class of the form with `get_form_class()` and instance it with `get_form()` (more on them later). The `get()` method then just calls the `render_to_response()` method to render a template, passing the context produced by the `get_context_data()` method. Note that the context receives the form as built by the `get_form()` method.

The `post()` method does not directly render the template since it has to process incoming data before doing this last step. Instead the validation of the form is performed through its `is_valid()` method and the two methods `form_valid()` and `form_invalid()` are called depending on the result of the test. See the [official documentation](https://docs.djangoproject.com/en/1.5/ref/forms/validation/) for more information about form validation.

Please note that the behaviour of these classes follows the same pattern of that used in `ListView` and `DetailView` as described in the previous two posts.

The `ProcessFormView` class inherits from `View`, which was already described in depth in the first two posts of this series; there you can find the `as_view()` and `dispatch()` method that are the foundation of the CBVs system.

## The form workflow - part I

The inheritance path that starts with `ProcessFormView` spans all the classes that deal with the incoming request, telling GET and POST methods apart. The second inheritance path we can follow from `BaseCreateView` leads to `ModelFormMixin`, which is defined at [views/generic/edit.py#L75](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L75). This path contains the classes that implement the form management methods. The first two methods that deal with the form are `get_form_class()` and `get_form()` that we encountered when discussing the `get()` and `post()` methods.

The `get_form_class()` tries to get the form model from the `self.form_class` attribute, and if this is not defined tries to extract the model from the `self.model` or from the queryset. Then it returns a suitable modelform, using a factory defined in [forms/models.py](https://github.com/django/django/blob/stable/1.5.x/django/forms/models.py).

The `get_form()` method is defined in `FormMixin` ([views/generic/edit.py#L10](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L10)) and instances the form class with the keywords returned by `get_form_kwargs()`, implemented in [views/generic/edit.py#L100](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L100). This last method is quite important as we are going to discover in a short while, since it has a big role in the double interaction that happens with POST requests.

The first implementation of the `get_form_kwargs()` method that we find in the ancestors tree is in `ModelFormMixin` ([views/generic/edit.py#L100](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L100)), but this immediately calls the same method defined in `FormMixin` ([views/generic/edit.py#L10](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L10)). The code is

``` python
def get_form_kwargs(self):
    kwargs = {'initial': self.get_initial()}
    if self.request.method in ('POST', 'PUT'):
        kwargs.update({
            'data': self.request.POST,
            'files': self.request.FILES,
        })
    return kwargs
```

The first value of the form keywords dictionary is the copy of the `self.initial` dictionary, returned by `get_initial()` as stated by [the official documentation](https://docs.djangoproject.com/en/1.5/ref/forms/api/#dynamic-initial-values). Then, if the method of the request being processed is POST or PUT, the keywords are updated with the content of the request itself, i.e. posted data and uploaded files. This is used to initialize the form object itself as you can see at [forms/forms.py#L77](https://github.com/django/django/blob/stable/1.5.x/django/forms/forms.py#L77), and I am going to describe this mechanism later.

After this method returns its dictionary, the execution continues in `ModelFormMixin`. The code of that method is

``` python
def get_form_kwargs(self):
    kwargs = super(ModelFormMixin, self).get_form_kwargs()
    kwargs.update({'instance': self.object})
    return kwargs
```

that just adds `self.object` under the `instance` key of the keywords dictionary. We already met `self.object` when discussing `DetailView`, where it contained the result of the queryset, i.e. the object being shown by the view.

What is `self.object` now? Among the ancestors of our `CreateView` class, `BaseCreateView` defines `self.object` as `None` so for the moment we can leave it aside. It will come to the rescue later, when we will discuss update and deletion forms, so do not forget it.

The last things we find in the `get()` method, just after `get_form_class()` and `get_form()`, is `get_context_data()`. As happened in `ListView` and `DetailView`, this method builds a dictionary (the context) that is used to render a template. You can find the implementation of `get_context_data()` at [views/generic/edit.py#L130](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L130). As you can see, since `self.object` has been set to `None`, the context contains only the instanced form under the `form` keyword (inserted at [views/generic/edit.py#L155](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L155)).

Let's recap the process until here.

1. The URL dispatcher requests with GET a page containing a form.
2. The `get()` method of `ProcessFormView` finds the form class of choice through `get_form_class()`
3. The form class is instanced by `get_form()` with the values contained in the `self.initial` dictionary
4. At this point a template is rendered with a context returned by `get_context_data()` as usual. The context contains the form.

## The form workflow - part II

Now the user obtained the requested page and is facing an empty form. Once the form has been filled, he or she clicks the submit button and a new HTTP request reaches the server, this time carrying the POST method and a set of data taken from the input fields of the form itself. Our view shall now handle the second interaction step, the one that in functional views is usually managed by the part starting with `if request.method == 'POST':`.

As we already know, the incoming request is processed by the `post()` method of `ProcessFormView`, which works like the `get()` method in its first part, calling `get_form_class()` and `get_form()`. This latter method now deals with a POST request, so the code of `get_form_kwargs()` in `FormMixin` ([views/generic/edit.py#L10](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L10)) adds to the keywords dictionary the submitted data with the `data` key and the uploaded files under the `files` key. Why does Django do this? Well, as you can see at [forms/forms.py#L77](https://github.com/django/django/blob/stable/1.5.x/django/forms/forms.py#L77) a Django form can be instanced with an optional `data` keyword, which is stored inside the form object for the subsequent validation phase.

So now the form is bound (that is, it contains some data or files - see [forms/forms.py#L80](https://github.com/django/django/blob/stable/1.5.x/django/forms/forms.py#L80)). The `post()` method now tests the result of `is_valid()` and acts accordingly calling either `form_valid()` or `form_invalid()`. Pay attention that, while `is_valid()` is a method of the form itself, the two latter methods belong to the `BaseCreateView`, defined by the same ancestor classes that implement `get_form_kwargs()`, `ModelFormMixin` and `FormMixin`.

The former class implements it at [views/generic/edit.py#L123](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L123) and puts the result of `form.save()` into `self.object`. Remember that `self.object` is appended to the context under the `object` key by the `get_context_data()` method, as shown in the previous section. The `form.save()` method for modelforms is defined by `BaseModelForm` at [forms/models.py#L357](https://github.com/django/django/blob/stable/1.5.x/django/forms/models.py#L357) and basically saves the instance of the Django model connected with the modelform, that is implements the actual creation at the base of the `CreateView` form view. As `form.save()` returns the object saved to the database, it makes sense to store it in `self.object` and pass it to the template.

The execution of `form_valid()` continues with the implementation in the `FormMixin` class at [views/generic/edit.py#L61](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L61). This method returns a `HttpResponseRedirect` object, which is the way you make the browser point to the given URL in Django. In this case the URL is given by `self.get_success_url()` which tries to return `self.success_url` if defined, otherwise returns the result of `get_absolute_url()` for the fresh-made object.

On the other hand, `form_invalid()` at [views/generic/edit.py#L67](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L67) deals with the case of a form containing some errors and simply calls `render_to_response()` passing it the context with the compiled form under the `form` key.

## Update and Delete operations

This rather rich code tour unveiled the inner mechanism of the `CreateView` class, which can be used to create a new object in the database. The `UpdateView` and `DeleteView` classes follow a similar path, with minor changes to perform the different action they are implementing.

`UpdateView` wants to show the form already filled with values, so it instances `self.object` before processing the request ([views/generic/edit.py#L210](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L210)). This makes the object available in the keywords dictionary under the `instance` key ([views/generic/edit.py#L105](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L105)), which is used by modelforms to initialize the data ([forms/models.py#L244](https://github.com/django/django/blob/stable/1.5.x/django/forms/models.py#L244)). The `form.save()` method of `BaseModelForm` is smart enough to understand if the object has been created or just changed ([forms/models.py#L365](https://github.com/django/django/blob/stable/1.5.x/django/forms/models.py#L365) so the `post()` method of `UpdateView` works just like the one of `CreateView`.

`DeleteView` is a bit different from `CreateView` and `UpdateView`. As [the official documentation](https://docs.djangoproject.com/en/1.5/ref/class-based-views/generic-editing/#deleteview) states, if called with a GET method it shows a confirmation page that POSTs to the same URL. So, as for the GET requests, `DeleteView` just uses the `get()` method defined by its ancestor `BaseDetailView` ([views/generic/detail.py#L103](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/detail.py#L103)), which renders the template putting the object in the context. When called with a POST request, the view uses the `post()` method defined by `DeletionMixin` ([views/generic/edit.py#L233](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L233), which in turn just calls the `delete()` method of the same class ([views/generic/edit.py#L239](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L239)). This performs the deletion on the database and redirects to the success URL.

## Conclusion

As you can see, the structure behind the current implementation of Django class-based form views is rather complex. This allows the user to achieve complex behaviours like the CUD operations just by defining a couple of classes as I did in the simple example at the beginning of the post. Most of the time, however, such a simplification makes it difficult for the programmer to understand how to achieve the desired changes to the class behaviour. So the purpose of this big tour I made inside the Django source code was to give an insight of what methods are called in the lifetime of your HTTP request so that you can better identify what methods you need to override.

When performing special actions that fall outside the standard CUD operations you better inherit from `FormView` ([views/generic/edit.py#L181](https://github.com/django/django/blob/stable/1.5.x/django/views/generic/edit.py#L181)). The first thing to do is to check if and how you need to customize the `get()` and `post()` methods; remember that you either need to implement the full behaviour of those methods or make you changes and call the parent implementation. If this is not enough for your application consider overriding one of the more dedicated methods, such as `get_form_kwargs()` or `form_valid()`.

This post ends the series "Digging Up Django Class-based Views". Stay tuned for other [upcoming articles on Django](/categories/django/)!

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post or to ask for an in-depth analysis of some topic. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.

## Previous articles

* [Digging Up Django Class-based Views - 1](/blog/2013/10/28/digging-up-django-class-based-views-1)

* [Digging Up Django Class-based Views - 2](/blog/2013/12/11/digging-up-django-class-based-views-2)
