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
