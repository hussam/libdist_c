import pyerl
from gevent import Greenlet
from erlport import Port, Protocol, Atom

def erlang_interface(cls):
    wrapped_class = cls()
    wrapped_class.run(Port(use_stdio=True, packet=1))

class LibDistState(object):
    state = None

class LibDistServer(Protocol):
    
    def __init__(self):
        self.server = None
        self.private = LibDistState()
    
    def handle(self, port, message=None):
        result = tuple([Atom('error'), Atom('no_message')])
        if message: 
            result = tuple([Atom('error'), Atom('no_function_called')])
            func = message[0]
            func_args = message[1:]
            if func == u'new':
                starting_state = func_args[0]
                result = self.new(starting_state)
            elif func == u'cmd':
                state = self.private.state
                func_args = message[2:]
                cmd = func_args
                result = self.cmd(state, cmd)
            elif func == u'fork':
                instance = func_args[0]
                fork_node = func_args[1]
                fork_args = func_args[2]
                result = self.fork(instance, fork_node, fork_args)
            elif func == u'is_mutating':
                cmd = func_args
                result = self.is_mutating(cmd)
            elif func == u'stop':
                instance = func_args[0]
                reason = func_args[1]
                running = False
                result = self.stop(func_args)
        port.write(result)
        
    def start(self):
        host = "127.0.0.1"
        name = "libdistpython"
        node = name + "@" + host
        cookie = "TESTCOOKIE"
        self.sock = pyerl.connect_xinit(host, name, node, "127.0.0.1", cookie, 1)
        self.g = Greenlet.spawn(self.handle)
                
    def stop(self):
        self.g.kill()
        pyerl.stop_connection(self.sock)
    
    def new(self, state):
        raise NotImplementedError("The funciton load_functions() needs to be implemented!")
    
    def cmd(self, state, command):
        raise NotImplementedError("The funciton load_functions() needs to be implemented!")
    
    def fork(self, instance, fork_node, fork_args):
        raise NotImplementedError("The funciton load_functions() needs to be implemented!")
    
    def is_mutating(self, command):
        raise NotImplementedError("The funciton load_functions() needs to be implemented!")
    
    def stop(self, instance, reason):
        raise NotImplementedError("The funciton load_functions() needs to be implemented!")