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
