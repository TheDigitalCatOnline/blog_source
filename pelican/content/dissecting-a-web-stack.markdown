Title: Dissecting a Web stack
Date: 2020-02-16 15:00:00 +0000
Modified: 2020-10-27 08:30:00 +0000
Category: Programming
Tags: architectures, concurrent programming, cryptography, infrastructure, Flask, Django, Python, SSL, HTTP, WWW, AWS
Authors: Leonardo Giordani
Slug: dissecting-a-web-stack
Image: dissecting-a-web-stack
Summary: A layer-by-layer review of the components of a web stack and the reasons behind them

> It was gross. They wanted me to dissect a frog.
> 
> (Beetlejuice, 1988)

## Introduction

Having recently worked with young web developers who were exposed for the first time to proper production infrastructure, I received many questions about the various components that one can find in the architecture of a "Web service". These questions clearly expressed the confusion (and sometimes the frustration) of developers who understand how to create endpoints in a high-level language such as Node.js or Python, but were never introduced to the complexity of what happens between the user's browser and their framework of choice. Most of the times they don't know why the framework itself is there in the first place.

The challenge is clear if we just list (in random order), some of the words we use when we discuss (Python) Web development: HTTP, cookies, web server, Websockets, FTP, multi-threaded, reverse proxy, Django, nginx, static files, POST, certificates, framework, Flask, SSL, GET, WSGI, session management, TLS, load balancing, Apache.

In this post, I want to review all the words mentioned above (and a couple more) trying to build a production-ready web service from the ground up. I hope this might help young developers to get the whole picture and to make sense of these "obscure" names that senior developers like me tend to drop in everyday conversations (sometimes arguably out of turn).

As the focus of the post is the global architecture and the reasons behind the presence of specific components, the example service I will use will be a basic HTML web page. The reference language will be Python but the overall discussion applies to any language or framework.

My approach will be that of first stating the rationale and then implementing a possible solution. After this, I will point out missing pieces or unresolved issues and move on with the next layer. At the end of the process, the reader should have a clear picture of why each component has been added to the system.

## The perfect architecture

A very important underlying concept of system architectures is that there is no _perfect solution_ devised by some wiser genius, that we just need to apply. Unfortunately, often people mistake design patterns for such a "magic solution". The "Design Patterns" original book, however, states that

> Your design should be specific to the problem at hand but also general enough to address future problems and requirements. You also want to avoid redesign, or at least minimize it.

And later

> Design patterns make it easier to reuse successful designs and architectures. [...] Design patterns help you choose design alternatives that make a system reusable and avoid alternatives that compromise reusability.

The authors of the book are discussing Object-oriented Programming, but these sentences can be applied to any architecture. As you can see, we have a "problem at hand" and "design alternatives", which means that the most important thing to understand is the requirements, both the present and future ones. Only with clear requirements in mind, one can effectively design a solution, possibly tapping into the great number of patterns that other designers already devised.

A very last remark. A web stack is a complex beast, made of several components and software packages developed by different programmers with different goals in mind. It is perfectly understandable, then, that such components have some degree of superposition. While the division line between theoretical layers is usually very clear, in practice the separation is often blurry. Expect this a lot, and you will never be lost in a web stack anymore.

## Some definitions

Let's briefly review some of the most important concepts involved in a Web stack, the protocols.

### TCP/IP

TCP/IP is a network protocol, that is, a _set of established rules_ two computers have to follow to get connected over a physical network to exchange messages. TCP/IP is composed of two different protocols covering two different layers of the OSI stack, namely the Transport (TCP) and the Network (IP) ones. TCP/IP can be implemented on top of any physical interface (Data Link and Physical OSI layers), such as Ethernet and Wireless. Actors in a TCP/IP network are identified by a _socket_, which is a tuple made of an IP address and a port number.

As far as we are concerned when developing a Web service, however, we need to be aware that TCP/IP is a _reliable_ protocol, which in telecommunications means that the protocol itself takes care or retransmissions when packets get lost. In other words, while the speed of the communication is not granted, we can be sure that once a message is sent it will reach its destination without errors.

### HTTP

TCP/IP can guarantee that the raw bytes one computer sends will reach their destination, but this leaves completely untouched the problem of how to send meaningful information. In particular, in 1989 the problem Tim Barners-Lee wanted to solve was how to uniquely name hypertext resources in a network and how to access them.

HTTP is the protocol that was devised to solve such a problem and has since greatly evolved. With the help of other protocols such as WebSocket, HTTP invaded areas of communication for which it was originally considered unsuitable such as real-time communication or gaming.

At its core, HTTP is a protocol that states the format of a text request and the possible text responses. The initial version 0.9 published in 1991 defined the concept of URL and allowed only the GET operation that requested a specific resource. HTTP 1.0 and 1.1 added crucial features such as headers, more methods, and important performance optimisations. At the time of writing the adoption of HTTP/2 is around 45% of the websites in the world, and HTTP/3 is still a draft.

The most important feature of HTTP we need to keep in mind as developers is that it is a _stateless_ protocol. This means that the protocol doesn't require the server to keep track of the state of the communication between requests, basically leaving session management to the developer of the service itself.

Session management is crucial nowadays because you usually want to have an authentication layer in front of a service, where a user provides credentials and accesses some private data. It is, however, useful in other contexts such as visual preferences or choices made by the user and re-used in later accesses to the same website. Typical solutions to the session management problem of HTTP involve the use of cookies or session tokens.

### HTTPS

Security has become a very important word in recent years, and with a reason. The amount of sensitive data we exchange on the Internet or store on digital devices is increasing exponentially, but unfortunately so is the number of malicious attackers and the level of damage they can cause with their actions. The HTTP protocol is inherently

HTTP is inherently insecure, being a plain text communication between two servers that usually happens on a completely untrustable network such as the Internet. While security wasn't an issue when the protocol was initially conceived, it is nowadays a problem of paramount importance, as we exchange private information, often vital for people's security or for businesses. We need to be sure we are sending information to the correct server and that the data we send cannot be intercepted.

HTTPS solves both the problem of tampering and eavesdropping, encrypting HTTP with the Transport Layer Security (TLS) protocol, that also enforces the usage of digital certificates, issued by a trusted authority. At the time of writing, approximately 80% of websites loaded by Firefox use HTTPS by default. When a server receives an HTTPS connection and transforms it into an HTTP one it is usually said that it _terminates TLS_ (or SSL, the old name of TLS).

### WebSocket

One great disadvantage of HTTP is that communication is always initiated by the client and that the server can send data only when this is explicitly requested. Polling can be implemented to provide an initial solution, but it cannot guarantee the performances of proper full-duplex communication, where a channel is kept open between server and client and both can send data without being requested. Such a channel is provided by the WebSocket protocol.

WebSocket is a killer technology for applications like online gaming, real-time feeds like financial tickers or sports news, or multimedia communication like conferencing or remote education.

It is important to understand that WebSocket is not HTTP, and can exist without it. It is also true that this new protocol was designed to be used on top of an existing HTTP connection, so a WebSocket communication is often found in parts of a Web page, which was originally retrieved using HTTP in the first place.

## Implementing a service over HTTP

Let's finally start discussing bits and bytes. The starting point for our journey is a service over HTTP, which means there is an HTTP request-response exchange. As an example, let us consider a GET request, the simplest of the HTTP methods.

```
GET / HTTP/1.1
Host: localhost
User-Agent: curl/7.65.3
Accept: */*
```

As you can see, the client is sending a pure text message to the server, with the format specified by the HTTP protocol. The first line contains the method name (`GET`), the URL (`/`) and the protocol we are using, including its version (`HTTP/1.1`). The remaining lines are called _headers_ and contain metadata that can help the server to manage the request. The complete value of the `Host` header is in this case `localhost:80`, but as the standard port for HTTP services is 80, we don't need to specify it.

If the server `localhost` is _serving_ HTTP (i.e. running some software that understands HTTP) on port 80 the response we might get is something similar to

```
HTTP/1.0 200 OK
Date: Mon, 10 Feb 2020 08:41:33 GMT
Content-type: text/html
Content-Length: 26889
Last-Modified: Mon, 10 Feb 2020 08:41:27 GMT
 
<!DOCTYPE HTML>
<html>
...
</html>
```

As happened for the request, the response is a text message, formatted according to the standard. The first line mentions the protocol and the status of the request (`200` in this case, that means success), while the following lines contain metadata in various headers. Finally, after an empty line, the message contains the resource the client asked for, the source code of the base URL of the website in this case. Since this HTML page probably contains references to other resources like CSS, JS, images, and so on, the browser will send several other requests to gather all the data it needs to properly show the page to the user.

So, the first problem we have is that of implementing a server that understands this protocol and sends a proper response when it receives an HTTP request. We should try to load the requested resource and return either a success (HTTP 200) if we can find it, or a failure (HTTP 404) if we can't.

## 1 Sockets and parsers

### 1.1 Rationale

TCP/IP is a network protocol that works with _sockets_. A socket is a tuple of an IP address (unique in the network) and a port (unique for a specific IP address) that the computer uses to communicate with others. A socket is a file-like object in an operating system, that can be thus _opened_ and _closed_, and that we can _read_ from or _write_ to. Socket programming is a pretty low-level approach to the network, but you need to be aware that every software in your computer that provides network access has ultimately to deal with sockets (most probably through some library, though).

Since we are building things from the ground up, let's implement a small Python program that opens a socket connection, receives an HTTP request, and sends an HTTP response. As port 80 is a "low port" (a number smaller than 1024), we usually don't have permissions to open sockets there, so I will use port 8080. This is not a problem for now, as HTTP can be served on any port.

### 1.2 Implementation

Create the file `server.py` and type this code. Yes, **type it**, don't just copy and paste, you will not learn anything otherwise.

``` python
import socket

## Create a socket instance
## AF_INET: use IP protocol version 4
## SOCK_STREAM: full-duplex byte stream
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

## Allow reuse of addresses
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

## Bind the socket to any address, port 8080, and listen
s.bind(('', 8080))
s.listen()

## Serve forever
while True:
    # Accept the connection
    conn, addr = s.accept()

    # Receive data from this socket using a buffer of 1024 bytes
    data = conn.recv(1024)

    # Print out the data
    print(data.decode('utf-8'))

    # Close the connection
    conn.close()
```

This little program accepts a connection on port 8080 and prints the received data on the terminal. You can test it executing it and then running `curl localhost:8080` in another terminal. You should see something like

``` sh
$ python3 server.py 
GET / HTTP/1.1
Host: localhost:8080
User-Agent: curl/7.65.3
Accept: */*
```

The server keeps running the code in the `while` loop, so if you want to terminate it you have to do it with Ctrl+C. So far so good, but this is not an HTTP server yet, as it sends no response; you should actually receive an error message from curl that says `curl: (52) Empty reply from server`.

Sending back a standard response is very simple, we just need to call `conn.sendall` passing the raw bytes. A minimal HTTP response contains the protocol and the status, an empty line, and the actual content, for example

``` txt
HTTP/1.1 200 OK

Hi there!
```

Our server becomes then

``` python
import socket

## Create a socket instance
## AF_INET: use IP protocol version 4
## SOCK_STREAM: full-duplex byte stream
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

## Allow reuse of addresses
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

## Bind the socket to any address, port 8080, and listen
s.bind(('', 8080))
s.listen()

## Serve forever
while True:
    # Accept the connection
    conn, addr = s.accept()

    # Receive data from this socket using a buffer of 1024 bytes
    data = conn.recv(1024)

    # Print out the data
    print(data.decode('utf-8'))

    conn.sendall(bytes("HTTP/1.1 200 OK\n\nHi there!\n", 'utf-8'))

    # Close the connection
    conn.close()
```

At this point, we are not really responding to the user's request, however. Try different curl command lines like `curl localhost:8080/index.html` or `curl localhost:8080/main.css` and you will always receive the same response. We should try to find the resource the user is asking for and send that back in the response content.

This version of the HTTP server properly extracts the resource and tries to load it from the current directory, returning either a success of a failure

``` python
import socket
import re

## Create a socket instance
## AF_INET: use IP protocol version 4
## SOCK_STREAM: full-duplex byte stream
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

## Allow reuse of addresses
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

## Bind the socket to any address, port 8080, and listen
s.bind(('', 8080))
s.listen()

HEAD_200 = "HTTP/1.1 200 OK\n\n"
HEAD_404 = "HTTP/1.1 404 Not Found\n\n"

## Serve forever
while True:
    # Accept the connection
    conn, addr = s.accept()

    # Receive data from this socket using a buffer of 1024 bytes
    data = conn.recv(1024)

    request = data.decode('utf-8')

    # Print out the data
    print(request)

    resource = re.match(r'GET /(.*) HTTP', request).group(1)
    try:
        with open(resource, 'r') as f:
            content = HEAD_200 + f.read()
        print('Resource {} correctly served'.format(resource))
    except FileNotFoundError:
        content = HEAD_404 + "Resource /{} cannot be found\n".format(resource)
        print('Resource {} cannot be loaded'.format(resource))

    print('--------------------')

    conn.sendall(bytes(content, 'utf-8'))

    # Close the connection
    conn.close()
```

As you can see this implementation is extremely simple. If you create a simple local file named `index.html` with this content

``` html
<head>
    <title>This is my page</title>
    <link rel="stylesheet" href="main.css">
</head>
<html>
    <p>Some random content</p>
</html>
```

and run `curl localhost:8080/index.html` you will see the content of the file. At this point, you can even use your browser to open `http://localhost:8080/index.html` and you will see the title of the page and the content. A Web browser is a software capable of sending HTTP requests and of interpreting the content of the responses if this is HTML (and many other file types like images or videos), so it can _render_ the content of the message. The browser is also responsible of retrieving the missing resources needed for the rendering, so when you provide links to style sheets or JS scripts with the `<link>` or the `<script>` tags in the HTML code of a page, you are instructing the browser to send an HTTP GET request for those files as well.

The output of `server.py` when I access `http://localhost:8080/index.html` is

```
GET /index.html HTTP/1.1
Host: localhost:8080
User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:72.0) Gecko/20100101 Firefox/72.0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8
Accept-Language: en-GB,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
Upgrade-Insecure-Requests: 1
Pragma: no-cache
Cache-Control: no-cache


Resource index.html correctly served
--------------------
GET /main.css HTTP/1.1
Host: localhost:8080
User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:72.0) Gecko/20100101 Firefox/72.0
Accept: text/css,*/*;q=0.1
Accept-Language: en-GB,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
Referer: http://localhost:8080/index.html
Pragma: no-cache
Cache-Control: no-cache


Resource main.css cannot be loaded
--------------------
GET /favicon.ico HTTP/1.1
Host: localhost:8080
User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:72.0) Gecko/20100101 Firefox/72.0
Accept: image/webp,*/*
Accept-Language: en-GB,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
Pragma: no-cache
Cache-Control: no-cache


Resource favicon.ico cannot be loaded
--------------------
```

As you can see the browser sends rich HTTP requests, with a lot of headers, automatically requesting the CSS file mentioned in the HTML code and automatically trying to retrieve a favicon image.

### 1.3 Resources

These resources provide more detailed information on the topics discussed in this section

* [Python 3 Socket Programming HOWTO](https://docs.python.org/3/howto/sockets.html)
* [HTTP/1.1 Request format](https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5)
* [HTTP/1.1 Response format](https://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6)
* The source code of this example is available [here](https://github.com/lgiordani/dissecting-a-web-stack-code/tree/master/1_sockets_and_parsers)

### 1.4 Issues

It gives a certain dose of satisfaction to build something from scratch and discover that it works smoothly with full-fledged software like the browser you use every day. I also think it is very interesting to discover that technologies like HTTP, that basically run the world nowadays, are at their core very simple.

That said, there are many features of HTTP that we didn't cover with our simple socket programming. For starters, HTTP/1.0 introduced other methods after GET, such as POST that is of paramount importance for today's websites, where users keep sending information to servers through forms. To implement all 9 HTTP methods we need to properly parse the incoming request and add relevant functions to our code.

At this point, however, you might notice that we are dealing a lot with low-level details of the protocol, which is usually not the core of our business. When we build a service over HTTP we believe that we have the knowledge to properly implement some code that can simplify a certain process, be it searching for other websites, shopping for books or sharing pictures with friends. We don't want to spend our time understanding the subtleties of the TCP/IP sockets and writing parsers for request-response protocols. It is nice to see how these technologies work, but on a daily basis, we need to focus on something at a higher level.

The situation of our small HTTP server is possibly worsened by the fact that HTTP is a stateless protocol. The protocol doesn't provide any way to connect two successive requests, thus keeping track of the _state_ of the communication, which is the cornerstone of modern Internet. Every time we authenticate on a website and we want to visit other pages we need the server to remember who we are, and this implies keeping track of the state of the connection.

Long story short: to work as a proper HTTP server, our code should at this point implement all HTTP methods and cookies management. We also need to support other protocols like Websockets. These are all but trivial tasks, so we definitely need to add some component to the whole system that lets us focus on the business logic and not on the low-level details of application protocols.

## 2 Web framework

### 2.1 Rationale

Enter the Web framework!

As I discussed many times (see [the book on clean architectures]({filename}cabook.markdown) or [the relative post]({filename}clean-architectures-in-python-a-step-by-step-example.mau)) the role of the Web framework is that of _converting HTTP requests into function calls_, and function return values into HTTP responses. The framework's true nature is that of a layer that connects a working business logic to the Web, through HTTP and related protocols. The framework takes care of session management for us and maps URLs to functions, allowing us to focus on the application logic.

In the grand scheme of an HTTP service, this is what the framework is supposed to do. Everything the framework provides out of this scope, like layers to access DBs, template engines, and interfaces to other systems, is an addition that you, as a programmer, may find useful, but is not in principle part of the reason why we added the framework to the system. We add the framework because it acts as a layer between our business logic and HTTP.

### 2.2 Implementation

Thanks to Miguel Gringberg and his [amazing Flask mega-tutorial](https://blog.miguelgrinberg.com/post/the-flask-mega-tutorial-part-i-hello-world) I can set up Flask in seconds. I will not run through the tutorial here, as you can follow it on Miguel's website. I will only use the content of the first article (out of 23!) to create an extremely simple "Hello, world" application.

To run the following example you will need a virtual environment and you will have to `pip install flask`. Follow Miguel's tutorial if you need more details on this.

The `app/__init__.py` file is

``` python
from flask import Flask

application = Flask(__name__)

from app import routes
```

and the `app/routes.py` file is

``` python
from app import application


@application.route('/')
@application.route('/index')
def index():
    return "Hello, world!"
```

You can already see here the power of a framework in action. We defined an `index` function and connected it with two different URLs (`/` and `/index`) in 3 lines of Python. This leaves us time and energy to properly work on the business logic, that in this case is a revolutionary "Hello, world!". Nobody ever did this before.

Finally, the `service.py` file is

``` python
from app import application
```

Flask comes with a so-called development web server (do these words ring any bell now?) that we can run on a terminal

``` sh
$ FLASK_APP=service.py flask run
 * Serving Flask app "service.py"
 * Environment: production
   WARNING: This is a development server. Do not use it in a production deployment.
   Use a production WSGI server instead.
 * Debug mode: off
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

You can now visit the given URL with your browser and see that everything works properly. Remember that 127.0.0.1 is the special IP address that refers to "this computer"; the name `localhost` is usually created by the operating system as an alias for that, so the two are interchangeable. As you can see the standard port for Flask's development server is 5000, so you have to mention it explicitly, otherwise your browser would try to access port 80 (the default HTTP one). When you connect with the browser you will see some log messages about the HTTP requests

``` sh
127.0.0.1 - - [14/Feb/2020 14:54:27] "GET / HTTP/1.1" 200 -
127.0.0.1 - - [14/Feb/2020 14:54:28] "GET /favicon.ico HTTP/1.1" 404 -
```

You can recognise both now, as those are the same request we got with our little server in the previous part of the article.

### 2.3 Resources

These resources provide more detailed information on the topics discussed in this section

* [Miguel Gringberg's amazing Flask mega-tutorial](https://blog.miguelgrinberg.com/post/the-flask-mega-tutorial-part-i-hello-world)
* [What is localhost](https://en.wikipedia.org/wiki/Localhost)
* The source code of this example is available [here](https://github.com/lgiordani/dissecting-a-web-stack-code/tree/master/2_web_framework)

### 2.4 Issues

Apparently, we solved all our problems, and many programmers just stop here. They learn how to use the framework (which is a big achievement!), but as we will shortly discover, this is not enough for a production system. Let's have a closer look at the output of the Flask server. It clearly says, among other things

``` sh
   WARNING: This is a development server. Do not use it in a production deployment.
   Use a production WSGI server instead.
```

The main issue we have when we deal with any production system is represented by performances. Think about what we do with JavaScript when we minimise the code: we consciously obfuscate the code in order to make the file smaller, but this is done for the sole purpose of making the file faster to retrieve.

For HTTP servers the story is not very different. The Web framework usually provides a development Web server, as Flask does, which properly implements HTTP, but does it in a very inefficient way. For starters, this is a _blocking_ framework, which means that if our request takes seconds to be served (for example because the endpoint retrieves data from a very slow database), any other request will have to wait to be served in a queue. That ultimately means that the user will see a spinner in the browser's tab and just shake their head thinking that we can't build a modern website. Other performances concerns might be connected with memory management or disk caches, but in general, we are safe to say that this web server cannot handle any production load (i.e. multiple users accessing the web site at the same time and expecting good quality of service).

This is hardly surprising. After all, we didn't want to deal with TCP/IP connections to focus on our business, so we delegated this to other coders who maintain the framework. The framework's authors, in turn, want to focus on things like middleware, routes, proper handling of HTTP methods, and so on. They don't want to spend time trying to optimise the performances of the "multi-user" experience. This is especially true in the Python world (and somehow less true for Node.js, for example): Python is not heavily concurrency-oriented, and both the style of programming and the performances are not favouring fast, non-blocking applications. This is changing lately, with async and improvements in the interpreter, but I leave this for another post.

So, now that we have a full-fledged HTTP service, we need to make it so fast that users won't even notice this is not running locally on their computer.

## 3 Concurrency and façades

### 3.1 Rationale 

Well, whenever you have performance issues, just go for concurrency. Now you have many problems!
(see [here](https://twitter.com/davidlohr/status/288786300067270656?lang=en))

Yes, concurrency solves many problems and it's the source of just as much, so we need to find a way to use it in the safest and less complicated way. We basically might want to add a layer that runs the framework in some concurrent way, without requiring us to change anything in the framework itself.

And whenever you have to homogenise different things just create a layer of indirection. This solves any problem but one. (see [here](https://en.wikipedia.org/wiki/Fundamental_theorem_of_software_engineering))

So we need to create a layer that runs our service in a concurrent way, but we also want to keep it detached from the specific implementation of the service, that is independent of the framework or library that we are using.

### 3.2 Implementation

In this case, the solution is that of giving a _specification_ of the API that web frameworks have to expose, in order to be usable by independent third-party components. In the Python world, this set of rules has been named WSGI, the Web Server Gateway Interface, but such interfaces exist for other languages such as Java or Ruby. The "gateway" mentioned here is the part of the system outside the framework, which in this discussion is the part that deals with production performances. Through WSGI we are defining a way for frameworks to expose a common interface, leaving people interested in concurrency free to implement something independently.

If the framework is compatible with the gateway interface, we can add software that deals with concurrency and uses the framework through the compatibility layer. Such a component is a production-ready HTTP server, and two common choices in the Python world are Gunicorn and uWSGI.

Production-ready HTTP server means that the software understands HTTP as the development server already did, but at the same time pushes performances in order to sustain a bigger workload, and as we said before this is done through concurrency.

Flask is compatible with WSGI, so we can make it work with Gunicorn. To install it in our virtual environment run `pip install gunicorn` and set it up creating a file names `wsgi.py` with the following content

``` python
from app import application


if __name__ == "__main__":
    application.run()
```

To run Gunicorn specify the number of concurrent instances and the external port

``` sh
$ gunicorn --workers 3 --bind 0.0.0.0:8000 wsgi
[2020-02-12 18:39:07 +0000] [13393] [INFO] Starting gunicorn 20.0.4
[2020-02-12 18:39:07 +0000] [13393] [INFO] Listening at: http://0.0.0.0:8000 (13393)
[2020-02-12 18:39:07 +0000] [13393] [INFO] Using worker: sync
[2020-02-12 18:39:07 +0000] [13396] [INFO] Booting worker with pid: 13396
[2020-02-12 18:39:07 +0000] [13397] [INFO] Booting worker with pid: 13397
[2020-02-12 18:39:07 +0000] [13398] [INFO] Booting worker with pid: 13398
```

As you can see, Gunicorn has the concept of _workers_ which are a generic way to express concurrency. Specifically, Gunicorn implements a pre-fork worker model, which means that it (pre)creates a different Unix process for each worker. You can check this running `ps`

``` sh
$ ps ax | grep gunicorn
14919 pts/1    S+     0:00 ~/venv3/bin/python3 ~/venv3/bin/gunicorn --workers 3 --bind 0.0.0.0:8000 wsgi
14922 pts/1    S+     0:00 ~/venv3/bin/python3 ~/venv3/bin/gunicorn --workers 3 --bind 0.0.0.0:8000 wsgi
14923 pts/1    S+     0:00 ~/venv3/bin/python3 ~/venv3/bin/gunicorn --workers 3 --bind 0.0.0.0:8000 wsgi
14924 pts/1    S+     0:00 ~/venv3/bin/python3 ~/venv3/bin/gunicorn --workers 3 --bind 0.0.0.0:8000 wsgi
```

Using processes is just one of the two ways to implement concurrency in a Unix system, the other being using threads. The benefits and demerits of each solution are outside the scope of this post, however. For the time being just remember that you are dealing with multiple workers that process incoming requests asynchronously, thus implementing a non-blocking server, ready to accept multiple connections.

### 3.3 Resources

These resources provide more detailed information on the topics discussed in this section

* The [WSGI official documentation](https://wsgi.readthedocs.io/en/latest/index.html) and the [Wikipedia page
](https://en.wikipedia.org/wiki/Web_Server_Gateway_Interface)
* The homepages of [Gunicorn](https://gunicorn.org/) and [uWSGI](https://uwsgi-docs.readthedocs.io/en/latest/)
* A good entry point for your journey into the crazy world of concurrency: [multithreading](https://en.wikipedia.org/wiki/Multithreading_(computer_architecture)).
* The source code of this example is available [here](https://github.com/lgiordani/dissecting-a-web-stack-code/tree/master/3_concurrency_and_facades)

### 3.4 Issues

Using a Gunicorn we have now a production-ready HTTP server, and apparently implemented everything we need. There are still many considerations and missing pieces, though.

#### Performances (again)

Are 3 workers enough to sustain the load of our new killer mobile application? We expect thousands of visitors per minute, so maybe we should add some. But while we increase the amount of workers, we have to keep in mind that the machine we are using has a finite amount of CPU power and memory. So, once again, we have to focus on performances, and in particular on scalability: how can we keep adding workers without having to stop the application, replace the machine with a more powerful one, and restart the service?

#### Embrace change

This is not the only problem we have to face in production. An important aspect of technology is that it changes over time, as new and (hopefully) better solutions become widespread. We usually design systems dividing them as much as possible into communicating layers exactly because we want to be free to replace a layer with something else, be it a simpler component or a more advanced one, one with better performances or maybe just a cheaper one. So, once again, we want to be able to evolve the underlying system keeping the same interface, exactly as we did in the case of web frameworks.

#### HTTPS

Another missing part of the system is HTTPS. Gunicorn and uWSGI do not understand the HTTPS protocol, so we need something in front of them that will deal with the "S" part of the protocol, leaving the "HTTP" part to the internal layers.

#### Load balancers

In general, a _load balancer_ is just a component in a system that distributes work among a pool of workers. Gunicorn is already distributing load among its workers, so this is not a new concept, but we generally want to do it on a bigger level, among machines or among entire systems. Load balancing can be hierarchical and be structured on many levels. We can also assign more importance to some components of the system, flagging them as ready to accept more load (for example because their hardware is better). Load balancers are extremely important in network services, and the definition of load can be extremely different from system to system: generally speaking, in a Web service the number of connections is the standard measure of the load, as we assume that on average all connections bring the same amount of work to the system.

#### Reverse proxies

Load balancers are forward proxies, as they allow a client to contact any server in a pool. At the same time, a _reverse proxy_ allows a client to retrieve data produced by several systems through the same entry point. Reverse proxies are a perfect way to route HTTP requests to sub-systems that can be implemented with different technologies. For example, you might want to have part of the system implemented with Python, using Django and Postgres, and another part served by an AWS Lambda function written in Go and connected with a non-relational database such as DynamoDB. Usually, in HTTP services this choice is made according to the URL (for example routing every URL that begins with `/api/`).

#### Logic

We also want a layer that can implement a certain amount of logic, to manage simple rules that are not related to the service we implemented. A typical example is that of HTTP redirections: what happens if a user accesses the service with an `http://` prefix instead of `https://`? The correct way to deal with this is through an HTTP 301 code, but you don't want such a request to reach your framework, wasting resources for such a simple task.

## 4 The Web server

### 4.1 Rationale

The general label of _Web server_ is given to software that performs the tasks we discussed. Two very common choices for this part of the system are nginx and Apache, two open source projects that are currently leading the market. With different technical approaches, they both implement all the features we discussed in the previous section (and many more).

### 4.2 Implementation

To test nginx without having to fight with the OS and install too many packages we can use Docker. Docker is useful to simulate a multi-machine environment, but it might also be your technology of choice for the actual production environment (AWS ECS works with Docker containers, for example).

The base configuration that we will run is very simple. One container will contain the Flask code and run the framework with Gunicorn, while the other container will run nginx. Gunicorn will serve HTTP on the internal port 8000, not exposed by Docker and thus not reachable from our browser, while nignx will expose port 80, the traditional HTTP port.

In the same directory of the file `wsgi.py`, create a `Dockerfile`

``` dockerfile
FROM python:3.6

ADD app /app
ADD wsgi.py /

WORKDIR .
RUN pip install flask gunicorn
EXPOSE 8000
```

This starts from a Python Docker image, adds the `app` directory and the `wsgi.py` file, and installs Gunicorn. Now create a configuration for nginx in a file called `nginx.conf` in the same directory

``` nginx
server {
    listen 80;
    server_name localhost;

    location / {
        proxy_pass http://application:8000/;
    }
}
```

This defines a server that listens on port 80 and that connects all the URL starting with `/` with a server called `application` on port 8000, which is the container running Gunicorn.

Last, create a file `docker-compose.yml` that will describe the configuration of the containers.

``` yaml
version: "3.7"
services:
  application:
    build:
      context: .
      dockerfile: Dockerfile
    command: gunicorn --workers 3 --bind 0.0.0.0:8000 wsgi
    expose:
        - 8000

  nginx:
    image: nginx
    volumes:
     - ./nginx.conf:/etc/nginx/conf.d/default.conf
    ports:
      - 8080:80
    depends_on:
      - application
```

As you can see the name `application` that we mentioned in the nginx configuration file is not a magic string, but is the name we assigned to the Gunicorn container in the Docker Compose configuration. Please note that nginx listens on port 80 inside the container, but the port is published as 8080 on the host.

To create this infrastructure we need to install Docker Compose in our virtual environment through `pip install docker-compose`. I also created a file named `.env` with the name of the project

``` sh
COMPOSE_PROJECT_NAME=service
```

At this point you can run Docker Compose with `docker-compose up -d`

``` sh
$ docker-compose up -d
Creating network "service_default" with the default driver
Creating service_application_1 ... done
Creating service_nginx_1       ... done
```

If everything is working correctly, opening the browser and visiting `localhost:8080` should show you the HTML page Flask is serving.

Through `docker-compose logs` we can check what services are doing. We can recognise the output of Gunicorn in the logs of the service named `application`

``` sh
$ docker-compose logs application
Attaching to service_application_1
application_1  | [2020-02-14 08:35:42 +0000] [1] [INFO] Starting gunicorn 20.0.4
application_1  | [2020-02-14 08:35:42 +0000] [1] [INFO] Listening at: http://0.0.0.0:8000 (1)
application_1  | [2020-02-14 08:35:42 +0000] [1] [INFO] Using worker: sync
application_1  | [2020-02-14 08:35:42 +0000] [8] [INFO] Booting worker with pid: 8
application_1  | [2020-02-14 08:35:42 +0000] [9] [INFO] Booting worker with pid: 9
application_1  | [2020-02-14 08:35:42 +0000] [10] [INFO] Booting worker with pid: 10
```

but the one we are mostly interested with now is the service named `nginx`, so let's follow the logs in real-time with `docker-compose logs -f nginx`. Refresh the `localhost` page you visited with the browser, and the container should output something like

``` sh
$ docker-compose logs -f nginx
Attaching to service_nginx_1
nginx_1        | 192.168.192.1 - - [14/Feb/2020:08:42:20 +0000] "GET / HTTP/1.1" 200 13 "-" "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:72.0) Gecko/20100101 Firefox/72.0" "-"
```

which is the standard log format of nginx. It shows the IP address of the client (`192.168.192.1`), the connection timestamp, the HTTP request and the response status code (200), plus other information on the client itself.

Let's now increase the number of services, to see the load balancing mechanism in action. To do this, first we need to change the log format of nginx to show the IP address of the machine that served the request. Change the `nginx.conf` file adding the `log_format` and `access_log` options

``` nginx
log_format upstreamlog '[$time_local] $host to: $upstream_addr: $request $status';

server {
    listen 80;
    server_name localhost;

    location / {
        proxy_pass http://application:8000;
    }

    access_log /var/log/nginx/access.log upstreamlog;
}
```

The `$upstream_addr` variable is the one that contains the IP address of the server proxied by nginx. Now run `docker-compose down` to stop all containers and then `docker-compose up -d --scale application=3` to start them again

``` sh
$ docker-compose down
Stopping service_nginx_1       ... done
Stopping service_application_1 ... done
Removing service_nginx_1       ... done
Removing service_application_1 ... done
Removing network service_default
$ docker-compose up -d --scale application=3
Creating network "service_default" with the default driver
Creating service_application_1 ... done
Creating service_application_2 ... done
Creating service_application_3 ... done
Creating service_nginx_1       ... done
```

As you can see, Docker Compose runs now 3 containers for the `application` service. If you open the logs stream and visit the page in the browser you will now see a slightly different output

``` sh
$ docker-compose logs -f nginx
Attaching to service_nginx_1
nginx_1        | [14/Feb/2020:09:00:16 +0000] localhost to: 192.168.240.4:8000: GET / HTTP/1.1 200
```

where you can spot `to: 192.168.240.4:8000` which is the IP address of one of the application containers. Please note that the IP address you see might be different, as it depends on the Docker network settings. If you now visit the page again multiple times you should notice a change in the upstream address, something like

``` sh
$ docker-compose logs -f nginx
Attaching to service_nginx_1
nginx_1        | [14/Feb/2020:09:00:16 +0000] localhost to: 192.168.240.4:8000: GET / HTTP/1.1 200
nginx_1        | [14/Feb/2020:09:00:17 +0000] localhost to: 192.168.240.2:8000: GET / HTTP/1.1 200
nginx_1        | [14/Feb/2020:09:00:17 +0000] localhost to: 192.168.240.3:8000: GET / HTTP/1.1 200
nginx_1        | [14/Feb/2020:09:00:17 +0000] localhost to: 192.168.240.4:8000: GET / HTTP/1.1 200
nginx_1        | [14/Feb/2020:09:00:17 +0000] localhost to: 192.168.240.2:8000: GET / HTTP/1.1 200
```

This shows that nginx is performing load balancing, but to tell the truth this is happening through Docker's DNS, and not by an explicit action performed by the web server. We can verify this accessing the nginx container and running `dig application` (you need to run `apt update` and `apt install dnsutils` to install `dig`)

``` sh
$ docker-compose exec nginx /bin/bash
root@99c2f348140e:/# apt update
root@99c2f348140e:/# apt install -y dnsutils
root@99c2f348140e:/# dig application

; <<>> DiG 9.11.5-P4-5.1-Debian <<>> application
;; global options: +cmd
;; Got answer:
;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 7221
;; flags: qr rd ra; QUERY: 1, ANSWER: 3, AUTHORITY: 0, ADDITIONAL: 0

;; QUESTION SECTION:
;application.                   IN      A

;; ANSWER SECTION:
application.            600     IN      A       192.168.240.2
application.            600     IN      A       192.168.240.4
application.            600     IN      A       192.168.240.3

;; Query time: 1 msec
;; SERVER: 127.0.0.11#53(127.0.0.11)
;; WHEN: Fri Feb 14 09:57:24 UTC 2020
;; MSG SIZE  rcvd: 110
```

To see load balancing performed by nginx we can explicitly define two services and assign them different weights. Run `docker-compose down` and change the nginx configuration to

``` nginx
upstream app {
    server application1:8000 weight=3;
    server application2:8000;
}

log_format upstreamlog '[$time_local] $host to: $upstream_addr: $request $status';

server {
    listen 80;
    server_name localhost;

    location / {
        proxy_pass http://app;
    }

    access_log /var/log/nginx/access.log upstreamlog;
}
```

We defined here an `upstream` structure that lists two different services, `application1` and `application2`, giving to the first one a weight of 3. This mean that each 4 requests, 3 will be routed to the first service, and one to the second service. Now nginx is not just relying on the DNS, but consciously choosing between two different services.

Let's define the services accordingly in the Docker Compose configuration file

``` yml
version: "3"
services:
  application1:
    build:
      context: .
      dockerfile: Dockerfile
    command: gunicorn --workers 6 --bind 0.0.0.0:8000 wsgi
    expose:
        - 8000

  application2:
    build:
      context: .
      dockerfile: Dockerfile
    command: gunicorn --workers 3 --bind 0.0.0.0:8000 wsgi
    expose:
        - 8000

  nginx:
    image: nginx
    volumes:
     - ./nginx.conf:/etc/nginx/conf.d/default.conf
    ports:
      - 80:80
    depends_on:
      - application1
      - application2
```

I basically duplicated the definition of `application`, but the first service is running now 6 workers, just for the sake of showing a possible difference between the two. Now run `docker-compose up -d` and `docker-compose logs -f nginx`. If you refresh the page on the browser multiple times you will see something like

``` sh
$ docker-compose logs -f nginx
Attaching to service_nginx_1
nginx_1         | [14/Feb/2020:11:03:25 +0000] localhost to: 172.18.0.2:8000: GET / HTTP/1.1 200
nginx_1         | [14/Feb/2020:11:03:25 +0000] localhost to: 172.18.0.2:8000: GET /favicon.ico HTTP/1.1 404
nginx_1         | [14/Feb/2020:11:03:30 +0000] localhost to: 172.18.0.3:8000: GET / HTTP/1.1 200
nginx_1         | [14/Feb/2020:11:03:31 +0000] localhost to: 172.18.0.2:8000: GET / HTTP/1.1 200
nginx_1         | [14/Feb/2020:11:03:32 +0000] localhost to: 172.18.0.2:8000: GET / HTTP/1.1 200
nginx_1         | [14/Feb/2020:11:03:33 +0000] localhost to: 172.18.0.2:8000: GET / HTTP/1.1 200
nginx_1         | [14/Feb/2020:11:03:33 +0000] localhost to: 172.18.0.3:8000: GET / HTTP/1.1 200
nginx_1         | [14/Feb/2020:11:03:34 +0000] localhost to: 172.18.0.2:8000: GET / HTTP/1.1 200
nginx_1         | [14/Feb/2020:11:03:34 +0000] localhost to: 172.18.0.2:8000: GET / HTTP/1.1 200
nginx_1         | [14/Feb/2020:11:03:35 +0000] localhost to: 172.18.0.2:8000: GET / HTTP/1.1 200
nginx_1         | [14/Feb/2020:11:03:35 +0000] localhost to: 172.18.0.3:8000: GET / HTTP/1.1 200
```

where you can clearly notice the load balancing between `172.18.0.2` (`application1`) and `172.18.0.3` (`application2`) in action.

I will not show here an example of reverse proxy or HTTPS to prevent this post to become too long. You can find resources on those topics in the next section.

### 4.3 Resources

These resources provide more detailed information on the topics discussed in this section

* Docker Compose [official documentation](https://docs.docker.com/compose/)
* nginx [documentation](http://nginx.org/en/docs/): in particular the sections about [log_format](http://nginx.org/en/docs/http/ngx_http_log_module.html#log_format) and [upstream](http://nginx.org/en/docs/http/ngx_http_upstream_module.html#upstream) directives
* How to [configure logging](https://docs.nginx.com/nginx/admin-guide/monitoring/logging/) in nginx
* How to [configure load balancing](https://docs.nginx.com/nginx/admin-guide/load-balancer/http-load-balancer/) in nginx
* [Setting up an HTTPS Server](https://docs.nginx.com/nginx/admin-guide/security-controls/terminating-ssl-http/) with nginx and [how to created self-signed certificates](https://www.humankode.com/ssl/create-a-selfsigned-certificate-for-nginx-in-5-minutes)
* How to [create a reverse proxy](https://docs.nginx.com/nginx/admin-guide/web-server/reverse-proxy/) with nginx, the documentation of the [`location`](http://nginx.org/en/docs/http/ngx_http_core_module.html#location) directive and [some insights](https://www.digitalocean.com/community/tutorials/understanding-nginx-server-and-location-block-selection-algorithms) on the location choosing algorithms (one of the most complex parts of nginx)
* The source code of this example is available [here](https://github.com/lgiordani/dissecting-a-web-stack-code/tree/master/4_the_web_server)

### 4.4 Issues

Well, finally we can say that the job is done. Now we have a production-ready web server in front of our multi-threaded web framework and we can focus on writing Python code instead of dealing with HTTP headers.

Using a web server allows us to scale the infrastructure just adding new instances behind it, without interrupting the service. The HTTP concurrent server runs multiple instances of our framework, and the framework itself abstracts HTTP, mapping it to our high-level language.

## Bonus: cloud infrastructures

Back in the early years of the Internet, companies used to have their own servers on-premise, and system administrators used to run the whole stack directly on the bare operating system. Needless to say, this was complicated, expensive, and failure-prone.

Nowadays "the cloud" is the way to go, so I want to briefly mention some components that can help you run such a web stack on AWS, which is the platform I know the most and the most widespread cloud provider in the world at the time of writing.

### Elastic Beanstalk

This is the entry-level solution for simple applications, being a managed infrastructure that provides load balancing, auto-scaling, and monitoring. You can use several programming languages (among which Python and Node.js) and choose between different web servers like for example Apache or nginx. The components of an EB service are not hidden, but you don't have direct access to them, and you have to rely on configuration files to change the way they work. It's a good solution for simple services, but you will probably soon need more control.

[Go to Elastic Beanstalk](https://aws.amazon.com/elasticbeanstalk)

### Elastic Container Service (ECS)

With ECS you can run Docker containers grouping them in clusters and setting up auto-scale policies connected with metrics coming from CloudWatch. You have the choice of running them on EC2 instances (virtual machines) managed by you or on a serverless infrastructure called Fargate. ECS will run your Docker containers, but you still have to create DNS entries and load balancers on your own. You also have the choice of running your containers on Kubernetes using EKS (Elastic Kubernetes Service).

[Go to Elastic Container Service](https://aws.amazon.com/ecs/)

### Elastic Compute Cloud (EC2)

This is the bare metal of AWS, where you spin up stand-alone virtual machines or auto-scaling group of them. You can SSH into these instances and provide scripts to install and configure software. You can install here your application, web servers, databases, whatever you want. While this used to be the way to go at the very beginning of the cloud computing age I don't think you should go for it. There is so much a cloud provider can give you in terms of associated services like logs or monitoring, and in terms of performances, that it doesn't make sense to avoid using them. EC2 is still there, anyway, and if you run ECS on top of it you need to know what you can and what you can't do.

[Go to Elastic Compute Cloud](https://aws.amazon.com/ec2/)

### Elastic Load Balancing

While Network Load Balancers (NLB) manage pure TCP/IP connections, Application Load Balancers are dedicated to HTTP, and they can perform many of the services we need. They can reverse proxy through rules (that were recently improved) and they can terminate TLS, using certificates created in ACM (AWS Certificate Manager). As you can see, ALBs are a good replacement for a web server, even though they clearly lack the extreme configurability of a software. You can, however, use them as the first layer of load balancing, still using nginx or Apache behind them if you need some of the features they provide.

[Go to Elastic Load Balancing](https://aws.amazon.com/elasticloadbalancing/)

### CloudFront

CloudFront is a Content Delivery Network, that is a geographically-distributed cache that provides faster access to your content. While CDNs are not part of the stack that I discussed in this post I think it is worth mentioning CF as it can speed-up any static content, and also terminate TLS in connection with AWS Certificate Manager.

[Go to CloudFront](https://aws.amazon.com/cloudfront/)

## Conclusion

As you can see a web stack is a pretty rich set of components, and the reason behind them is often related to performances. There are a lot of technologies that we take for granted, and that fortunately have become easier to deploy, but I still believe a full-stack engineer should be aware not only of the existence of such layers, but also of their purpose and at least their basic configuration.

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.
