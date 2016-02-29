Title: Postage - a RabbitMQ-based component Python library
Date: 2013-07-25 15:50 +0100
Category: Projects
Tags: Python, Pika, RabbitMQ, Postage
Authors: Leonardo Giordani
Slug: postage-a-rabbitmq-based-component-python-library
Summary:


[Pika](https://github.com/pika/pika) is a wonderful pure Python implementation of the AMQP protocol. Using it you can exploit the full power of your RabbitMQ installation from your Python code.

When using pika to develop a component-based system I tried to write some code to simplify its use: the result is [Postage](https://github.com/lgiordani/postage), a Python library that provides higher level structures such as a message format, components fingerprint, rich producer and consumers.

Most notably it provides a handler mechanism for consumers that makes message processing a breeze.

Postage is freely available under the GPL2. It is based on the pika BlockingConnection since I had no experience with other adapters. If you want to hack it, feel free to [fork it on Github](https://github.com/lgiordani/postage) and submit a pull request.

## A simple ping example

I'll describe here a very simple example of a producer/consumer system using Postage; I'll write a server that answers ping messages and a program that sends them. First I will implement a simple server that receives ping messages without answering, to introduce the reader to the base structures, then I will evolve it.

To execute the program you need a working RabbitMQ system, check the RabbitMQ documentation to install and run it. Postage assumes that your system is configured with the standard values (a "/" virtualhost, "guest" user and password). If not check [this paragraph](https://github.com/lgiordani/postage#environment-variables) of the documentation.

#### Setting up the exchange

Put the following code in a `facilities.py` file:

``` python
from postage import messaging

class PingExchange(messaging.Exchange):
    """This is the exchange that receives ping messages."""
    name = "ping-exchange"
    exchange_type = "direct"
    passive = False
    durable = True
    auto_delete = False
```

This imports the messaging part of Postage and declares a `PingExchange`, which is a simple direct RabbitMQ exchange, which name is `ping-exchange`. Remember that in a AMQP system exchanges are unique by name and virtualhost, i.e. given a virtualhost the name of the exchange uniquely identifies it.

#### Setting up the producer

Just below the exchange object we declare a producer, a class that can send a given set of messages:

``` python
class PingProducer(messaging.GenericProducer):
    eks = [(PingExchange, 'ping_rk')]

    def build_message_ping(self):
        return messaging.MessageCommand('ping')
```

First of all our producer inherits from `GenericProducer`, a rich object that manages low-level stuff such as connection to the AMQP broker (RabbitMQ), exchange declaration and message creation.

The `eks` class attribute is a list of exchange/routing key couples (tuples); we list here all the exchanges that will receive our messages when the object will send them and for each exchange we give a routing key. Recall that routing keys are used to label messages so that the exchange can route them to the subscribing queues (according to the rules of the exchange type). Here, we declare that the messages of our producer are going to be sent to the `PingExchange` exchange with the `ping_rk` routing key.

Then we declare a `build_message_ping()` method, which simply builds a new message and returns it. This latter is a command message that in Postage lingo means a message that contains an action the receiver shall execute (a fire-and-forget call).

#### The producer

The program that sends ping messages is very straightforward; it shall declare a message producer and use it to send the message. Create the `send_ping.py` file and write the following code

``` python
from postage import messaging
import facilities

fingerprint = messaging.Fingerprint(name="ping_sender")

```

After the usual imports, I create a fingerprint for this program. As explained in [the documentation](https://github.com/lgiordani/postage#fingerprint), a fingerprint is a collection of useful information about the component that sends messages. It can be easily customized since all Postage objects expect it to be a dictionary, so any object that behaves like a dictionary works. The standard `Fingerprint` provided by Postage collects some useful properties from the OS and the RabbitMQ installation; here we customize the `name` value that otherwise would be set to `None`. The fingerprint, once loaded in a producer, will be automatically attached to any message the producer will send.

``` python
producer = facilities.PingProducer(fingerprint.as_dict())
producer.message_ping()

```

The `PingProducer` we declared in `facilities.py` is instanced, and its `message_ping()` method is invoked.
If you review the above paragraph you will notice that you never defined a `message_ping()` method; this is automatically implemented by the `GenericProducer` class from the `build_message_ping()` method. The class performs many actions under the hood: it executes some code to set up the correct RabbitMQ structures, calls your method to get the actual message data, attaches the fingerprint to the message, and serializes the message data. Eventually, the producer sends the message to the exchange defined in the class (`PingExchange`) with the linked routing key (`ping_rk`).

#### The server program

Now we will write a component that receives ping command messages and performs some action accordingly. Open a `receive_ping.py` file and write the following code

``` python
from postage import messaging
from postage import microthreads

import facilities

fingerprint = messaging.Fingerprint(name="ping_receiver")
```

that loads the modules we need and builds the fingerprint of this application. Creating a receiver means declaring a class that inherits from `MessageProcessor` and implements a method for each incoming message we want to process.

``` python
class PingReceiver(messaging.MessageProcessor):
    @messaging.MessageHandler('command', 'ping')
    def msg_ping(self, content):
        print "Got a ping!"
```

As you can see here the `msg_ping()` method is declared as a handler for the command message `ping`; the name of the method is arbitrary, but it has to accept one parameter, namely the content of the incoming message (more on this later). In this case, when the object receives a ping message it just prints out a string.

``` python
eqks = [(facilities.PingExchange, [('ping_queue', 'ping_rk')])]
receiver = PingReceiver(fingerprint.as_dict(), eqks,
                        None, messaging.global_vhost)
```

To start the receiver we have to connect it to an exchange; recall that the AMQP mechanism requires you to declare a queue and to connect it to an exchange through a key, which format depends on the exchange type. Being the `PingExchange` a direct exchange we want to connect to it with the exact routing key we want to match, that is `ping_rk`. The `eqks` structure is rather complex and may result overblown in such a simple context: it is a list of tuples in the form `(exchange_class, qk_list)` that links the given exchange class to a list of queues; this latter list contains tuples in the form `(queue_name, key)`. Each queue listed here connects to the exchange and fetches messages that match the linked key.

In this case, we simply subscribe the `facilities.PingExchange` exchange with a `ping_queue` queue receiving messages routed with the `ping_rk` key.

The receiver is then instanced. The arguments we pass are the fingerprint dictionary, the eqks we just discussed, a HUP tuple (Host, User, Password) to connect to RabbitMQ and the RabbitMQ virtualhost we want to use. In this case, we stick to the [default HUP](https://github.com/lgiordani/postage#environment-variables) and to the default virtualhost.

``` python
scheduler = microthreads.MicroScheduler()
scheduler.add_microthread(receiver)

for i in scheduler.main():
    pass
```

This code creates a scheduler and adds the receiver, which is a `microthreads.Microthread`, then starts the execution loop.

#### Execution

Open two different shells on your system and execute the receiver in the first

``` text
$ python receive_ping.py 
postage.messaging: global_vhost set to /
```

and the sender in the second

``` text
$ python send_ping.py 
postage.messaging: global_vhost set to /
$  
```

The receiver shall at this point notify that a message has been sent

``` text
$ python receive_ping.py 
postage.messaging: global_vhost set to /
Got a ping!
```

which is what we expected. You can stop the receiver with `Ctrl-C`, this kills the Pika connection somehow abruptly, but I am not going to implement in this article a good signal management.

#### Adding message parameters

Now we want to add a parameter to the message we send, namely the time at which the message was sent. To do this we make some changes to `facilities.py`

``` python
import time

[...]

class PingProducer(messaging.GenericProducer):
    eks = [(PingExchange, 'ping_rk')]

    def build_message_ping(self):
        return messaging.MessageCommand('ping')

    def build_message_timed_ping(self):
        return messaging.MessageCommand('timed_ping',
                                        parameters={'time':time.time()})
```

As you can see I just added the `build_message_timed_ping()` method, which sends a `timed_ping` command, but this time I added a `parameters` dictionary that encompasses all the parameters of the command. Remember that all the structures you put in a message are serialized in JSON by default so they must be processable by `json.dumps()`; if you need to send very complex structures you can customize Postage to use another encoder, either a customized JSON or a completely different one; see [the documentation](https://github.com/lgiordani/postage#encoder).

The receiver has to be modified accordingly:

``` python
class PingReceiver(messaging.MessageProcessor):
    @messaging.MessageHandler('command', 'ping')
    def msg_ping(self, content):
        print "Got a ping!"

    @messaging.MessageHandler('command', 'timed_ping')
    def msg_timed_ping(self, content):
        print "Got a timed ping! Time is %s" %(content['parameters']['time'])
```

Here the new method, `msg_timed_ping()`, prints a different message extracting the parameters from the message content.
Last, you need to add the actual call that sends the message to `send_ping.py`:

``` python
producer = facilities.PingProducer(fingerprint.as_dict())
producer.message_ping()
producer.message_timed_ping()
```

The execution shows that everything works as expected

``` text
$ python receive_ping.py 
postage.messaging: global_vhost set to /
Got a ping!
Got a timed ping! Time is 1374826309.06
```

#### Adding call parameters

If you want to allow the user to pass a parameter when sending the message, you just need to accept and use it in your `build_message_NAME()` method. In `facilities.py` add:

``` python
class PingProducer(messaging.GenericProducer):
    [...]
    def build_message_custom_ping(self, custom_value):
        return messaging.MessageCommand('custom_ping',
                                    parameters={'custom_value':custom_value})
```

Add a handler in the receiver (`receive_ping.py`):

``` python
class PingReceiver(messaging.MessageProcessor):
    [...]
    @messaging.MessageHandler('command', 'custom_ping')
    def msg_custom_ping(self, content):
        print "Got a custom ping! The custom value is %s"\
              %(content['parameters']['custom_value'])
```

And exploit it when sending the message (`send_ping.py`):

``` python
producer.message_custom_ping(("Just ping me", 1))
```

When you execute it you get:

``` text
$ python receive_ping.py 
postage.messaging: global_vhost set to /
Got a ping!
Got a timed ping! Time is 1374832738.18
Got a custom ping! The custom value is [u'Just ping me', 1]
```

Pay attention to JSON, which does not tell apart tuples from lists.

#### RPC calls to the rescue

The ping mechanism is not really working until the server answers the message. To answer incoming messages we can implement two different strategies; the first is the asynchronous one, which leverages fire-and-forget messages, the second uses RPC calls. While the first is simpler to implement at a system level (you just send messages as usual), it is complex on the user side since it requires the programmer to structure the whole program in an asynchronous way. The second approach, resembling usual function calls, is easier to understand and include in a program; it has many downsides and caveats, however, so do not abuse it.

For the sake of simplicity let us implement a RPC version of the ping mechanism. First we add a specific message to the producer

``` python
class PingProducer(messaging.GenericProducer):
    [...]
    def build_rpc_ping(self):
        return messaging.RpcCommand('ping')
```

Things are not very different from the previous cases here: we use the `build_rpc_NAME()` form of the method then we return an RpcCommand, instead of a MessageCommand. Beware that, alas!, nomenclature here is a little misleading: both are messages in the sense of "something that will be sent on the AMQP network", but while MessageCommand does not expect an answer, RpcCommand does.

I want to point out that the name of the message is `ping` just like the previous one; Postage tells the two messages apart using the name (`ping`), the type (`command`) and the category (`rpc` or `message`), although this latter is somewhat concealed.

The receiver needs a new handler to process the incoming RPC `ping` message:

``` python
class PingReceiver(messaging.MessageProcessor):
    [...]
    @messaging.RpcHandler('command', 'ping')
    def msg_rpc_ping(self, content, reply_func):
        print "Got a ping! Answering..."
        reply_func(messaging.MessageResult('Pong'))
```

Accordingly, there is an RPC version of `MessageHandler`, `RpcHandler`. The method has to accept an additional parameter that is a reply function; this latter can be called at any time from the method, allowing it to perform some cleanup after answering if needed. In this case, it simply sends a `MessageResult` object back with `'Pong'` as value.

In `send_ping.py` you can now make a remote call:

``` python
answer = producer.rpc_ping()

if answer.body['content']['type'] == 'success':
    print "Answer: %s" %(answer.body['content']['value'])
elif answer.body['content']['type'] == 'exception':
    print "An exception occoured! (%s)" %(answer.body['content']['value'])
```

The first part is straightforward: you call the RPC just like a local function. What you get is always a `MessageResult` object or derived (`MessageResultError` or `MessageResultException`). Be warned that the API here is awkward, to be indulgent. I wrote it, but probably the good-coder-in-me (TM) was on holiday that time; [I am going to fix it](https://github.com/lgiordani/postage/issues/1) in a short time.

Anyway, you have to check the answer to be sure that the call was successful; never, never, never trust RPC calls, network is in the middle and everything can happen (yes, even someone tripping over the network cable).

If the receiver is unreachable the producer waits some time and then tries the call again: by default it waits 30 seconds and tries again 4 times; after all that it returns a `MessageResultException` containing a `TimeoutError` exception. You can try it changing the decorator of `msg_rpc_ping()` to match `ping_other` (or whatever) instead of `ping`. After two minutes, you will get your exception. You can easily customize these values by setting the value of `GenericProducer.rpc_timeout` and `GenericProducer.max_retry`.

#### Handlers unleashed

Message handlers are powerful, but there is a couple of tricks more in Postage. The first one is `MessageHandlerFullBody` that you can use exactly like `MessageHandler`; the difference is that the decorated method does not receive the message content (the `content` key of the body) but the full body. You can leverage this to access the underlying message structure: this allows you to access the fingerprint included in the message, which contains precious information about the process that sent the message. Let's show how it works; add a new handler to the receiver:

``` python
class PingReceiver(messaging.MessageProcessor):
    [...]
    @messaging.MessageHandlerFullBody('command', 'ping')
    def msg_ping_full(self, body):
        fingerprint = body['fingerprint']
        print "Got a ping from %s running on %s with pid %s"\
              %(fingerprint['name'], fingerprint['host'], fingerprint['pid'])
```

Here, we handle the `ping` command, just like the method `msg_ping()` does; indeed nothing stops you to write more than a handler for a given message, but remember that they are processed in random order. Obviously we need to give the decorated method a different name, otherwise the second one will redefine the first one. Being decorated with `MessageHandlerFullBody` the method receives the full body of the message and can access the fingerprint.

Executing it we get:

``` text
$ python receive_ping.py 
postage.messaging: global_vhost set to staging
Got a ping from ping_sender running on yoda with pid 26812
Got a ping!
```

As we expected both handlers have been activated by the incoming message, and, not surprisingly, they have been processed out of order.

The second trick handlers have in store for you is the Handler class. Instead of decorating a method you can define a class that inherits from `Handler` and decorate that; this class shall at least define a `call()` method without arguments (aside from `self`) that will be executed when the relative message arrives. This class can access `self.data`, which is the data passed by the decorator (either the message content or the full body), `self.reply_func` that defaults to `None` for non-RPC messages, and `self.processor` that is the underlying `MessageProcessor` object hosting the handler.

To show how it works let's add another handler to the receiver:

``` python
class PingReceiver(messaging.MessageProcessor):
    [...]
    @messaging.MessageHandler('command', 'ping')
    class MsgPing(messaging.Handler):
        def call(self):
            print "Got a ping - processed by %s hosted by %s"\
                  %(self.__class__, self.processor.__class__)
```

You can see that the definition of a basic handler class is pretty simple. When executed this gives the following:

``` text
$ python receive_ping.py 
postage.messaging: global_vhost set to staging
Got a ping - processed by <class '__main__.MsgPing'>
             hosted by <class '__main__.PingReceiver'>
Got a ping from ping_sender running on yoda with pid 27596
Got a ping!
```

Leveraging the full body access and the class handlers you can write advanced filters on incoming messages, and add interesting features like runtime configuration of your handlers or configuration through incoming messages.

## Full code

This is the full code of the discussed examples.

``` python
import time
from postage import messaging

class PingExchange(messaging.Exchange):
    """This is the exchange that receives ping messages."""
    name = "ping-exchange"
    exchange_type = "direct"
    passive = False
    durable = True
    auto_delete = False

class PingProducer(messaging.GenericProducer):
    # Send messages to this exchange with this routing key
    eks = [(PingExchange, 'ping_rk')]

    # Send a 'ping' command
    def build_message_ping(self):
        return messaging.MessageCommand('ping')

    # Send a 'timed_ping' command
    # Parameters: time
    def build_message_timed_ping(self):
        return messaging.MessageCommand('timed_ping',
        parameters={'time':time.time()})

    # Send a 'custom_ping' command
    # Parameters: custom_value
    def build_message_custom_ping(self, custom_value):
        return messaging.MessageCommand('custom_ping',
        parameters={'custom_value':custom_value})

    # Send a 'ping' RPC command
    def build_rpc_ping(self):
        return messaging.RpcCommand('ping')
```
[source code](/code/postage/facilities.py)

``` python
from postage import messaging
import facilities

# Build the fingerprint of this application
fingerprint = messaging.Fingerprint(name="ping_sender")

# Instance the ping producer
producer = facilities.PingProducer(fingerprint.as_dict())

# Send a 'ping' command
producer.message_ping()

# Send a 'timed_ping' command
producer.message_timed_ping()

# Send a 'custom_ping' command
producer.message_custom_ping(("Just ping me", 1))

# Send a 'ping' RPC call
answer = producer.rpc_ping()
if answer.body['content']['type'] == 'success':
    print "Answer: %s" %(answer.body['content']['value'])
elif answer.body['content']['type'] == 'exception':
    print "An exception occoured! (%s)" %(answer.body['content']['value'])
```
[source code](/code/postage/send_ping.py)

``` python
from postage import messaging
from postage import microthreads

import facilities

# Build the fingerprint of this application
fingerprint = messaging.Fingerprint(name="ping_receiver")

class PingReceiver(messaging.MessageProcessor):
    # Process an incoming 'ping' command
    @messaging.MessageHandler('command', 'ping')
    def msg_ping(self, content):
        print "Got a ping!"

    # Process an incoming 'timed_ping' command
    @messaging.MessageHandler('command', 'timed_ping')
    def msg_timed_ping(self, content):
        print "Got a timed ping! Time is %s" %(content['parameters']['time'])

    # Process an incoming 'custom_ping' command
    @messaging.MessageHandler('command', 'custom_ping')
    def msg_custom_ping(self, content):
        print "Got a custom ping! The custom value is %s"\
        %(content['parameters']['custom_value'])

    # Process an incoming 'ping' RPC command
    @messaging.RpcHandler('command', 'ping')
    def msg_rpc_ping(self, content, reply_func):
        print "Got a ping! Answering..."
        reply_func(messaging.MessageResult('Pong'))

    # Process the full body of an incoming 'ping' command
    @messaging.MessageHandlerFullBody('command', 'ping')
    def msg_ping_full(self, body):
        fingerprint = body['fingerprint']
        print "Got a ping from %s running on %s with pid %s"\
        %(fingerprint['name'], fingerprint['host'], fingerprint['pid'])

    # Process an incoming 'ping' command with a class handler
    @messaging.MessageHandler('command', 'ping')
    class MsgPing(messaging.Handler):
        def call(self):
            print "Got a ping - processed by %s hosted by %s"\
        %(self.__class__, self.processor.__class__)

# Exchange/Queue/Key
eqks = [(facilities.PingExchange, [('ping_queue', 'ping_rk')])]

# Instance the receiver
receiver = PingReceiver(fingerprint.as_dict(), eqks, 
            None, messaging.global_vhost)

# Instance the scheduler and run the receiver
scheduler = microthreads.MicroScheduler()
scheduler.add_microthread(receiver)

for i in scheduler.main():
    pass
```
[source code](/code/postage/receive_ping.py)

## Conclusion

Postage aims to make it simple to write components in Python to fully exploit the power of RabbitMQ. It is highly customizable, and its handler mechanism keeps the code compact.

Even if the API is already in its third implementation, you can see that it is still not perfect so stay tuned for upcoming versions. Feel free to fork the project, to submit issues or pull request, or to contact me for any question.

Oh, did I remember to tell you to never trust RPC calls? =)
