import logging
import sys

from erlport import Port, Atom

from libdist.server import LibDistServer, erlang_interface
from pprint import pformat

class CalcServer(LibDistServer):
    def new(self, value):
        calcserver = logging.getLogger("CalcServer")
        calcserver.debug("Getting a new object")
        calcserver.debug("""state set to %d""" % value)
        #As long as something was passed set it to
        self.private.state = value
        return self.private.state
    
    def cmd(self, state, command):
        calcserver = logging.getLogger("CalcServer")
        calcserver.debug("Doing a calculation")
        calcserver.debug("Passed in state %d, Class' state is %d" % (state, self.private.state))
        result = None
        command = command[0]
        calcserver.debug("""Command~>%s""" % pformat(command))
        cmd = command[0]
        value = command[1]
        calcserver.debug("""Command[0]~>%s""" % cmd)
        calcserver.debug("""Command[1]~>%d""" % value)
        #Add
        if cmd == Atom('a'):
            calcserver.debug("Adding %s to %s" % (value, self.private.state))
            self.private.state += value
            result = self.private.state
        #Multiple
        elif cmd == Atom('m'):
            calcserver.debug("Multiplying %s with %s" % (value, self.private.state))
            self.private.state *= value
            result = self.private.state
        #Write
        elif cmd == Atom('w'):
            calcserver.debug("Writing %s instead of %s" % (value, self.private.state))
            self.private.state = value
            result = self.private.state
        #Read
        elif cmd == Atom('r'):
            calcserver.debug("Reading %s" % (self.private.state))
            result = self.private.state
        
        return result
    
    def fork(self, instance, fork_node, fork_args):
        calcserver = logging.getLogger("CalcServer")
        calcserver.debug("forking object")
        return self.new(instance)
    
    def is_mutating(self, command):
        calcserver = logging.getLogger("CalcServer")
        calcserver.debug("Checking if mutating")

        command = command[0]
        calcserver.debug("""Command~>%s""" % pformat(command))
        cmd = command[0]
        calcserver.debug("""Command[0]~>%s""" % cmd)
        if cmd in (Atom('a'), Atom('m'), Atom('w')):
            calcserver.debug("ADD, MULTIPLE, WRITE path")
            return True
        elif cmd in (Atom('r')):
            calcserver.debug("READ path")
            return False
        else:
            calcserver.debug("Command Argument is wrong")
            return -1
    
    def stop(self, instance, reason):
        calcserver = logging.getLogger("CalcServer")
        calcserver.debug("Stopping...")
        return None
        
if __name__ == "__main__":

    logging.basicConfig(level=logging.DEBUG)
    calcserver = logging.getLogger("CalcServer")
    calcserver.setLevel(logging.DEBUG)
        
    h1 = logging.FileHandler("calcserver.log")
    f = logging.Formatter("%(levelname)s %(asctime)s %(funcName)s %(lineno)d %(message)s")
    h1.setFormatter(f)
    h1.setLevel(logging.DEBUG)
    calcserver.addHandler(h1)

    
    
    def my_excepthook(type, value, tb):
        calcserver = logging.getLogger("CalcServer")
        calcserver.debug("In My Exception Handler")
        calcserver.error("Logging an uncaught exception",
                         exc_info=(type, value, tb))

        # the following line does the default (prints it to err)
        sys.__excepthook__(type, value, tb)

    sys.excepthook = my_excepthook    

    erlang_interface(CalcServer)
    #server = CalcServer()
    #server.run(Port(use_stdio=True, packet=1)) 
    
