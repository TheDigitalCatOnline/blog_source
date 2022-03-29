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
