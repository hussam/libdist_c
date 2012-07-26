import pyerl

def data_py2erl(data):
    cmd_list = []
    if isinstance(data, list) or isinstance(data, tuple):
        for item in data:
            if isinstance(item, str):
                cmd_list.append(pyerl.mk_string(item))
            elif isinstance(item, int):
                cmd_list.append(pyerl.mk_int(item))
            elif isinstance(item, float):
                cmd_list.append(pyerl.mk_float(item))
            elif isinstance(item, unicode):
                cmd_list.append(pyerl.mk_atom(item))
            elif isinstance(item, list):
                temp_list = []
                for inner_item in item:
                    if isinstance(inner_item, str):
                        temp_list.append(pyerl.mk_string(inner_item))
                    elif isinstance(inner_item, int):
                        temp_list.append(pyerl.mk_int(inner_item))
                    elif isinstance(inner_item, float):
                        temp_list.append(pyerl.mk_float(inner_item))
                    elif isinstance(inner_item, unicode):
                        temp_list.append(pyerl.mk_atom(inner_item))
                cmd_list.append(pyerl.mk_list(temp_list))
        
        return pyerl.mk_list(cmd_list)
        
    else:
        if isinstance(data, str):
            return pyerl.mk_string(data)
        elif isinstance(data, int):
            return pyerl.mk_int(data)
        elif isinstance(data, float):
            return pyerl.mk_float(data)
        elif isinstance(data, unicode):
            return pyerl.mk_atom(data)
    

class LibDistClient(object):
    def __init__(self, name, host, proxy, cookie, ip="127.0.0.1", retry=1000):
        self.host = host
        self.name = name
        self.node = name + "@" + host
        self.cookie = cookie
        self.ip = ip
        self.retry = pyerl.mk_int(retry)
        pyerl.connect_xinit(self.host, self.name, self.node, self.ip, self.cookie, retry)
        self.sock = pyerl.xconnect("127.0.0.1", proxy)
        eterm = pyerl.rpc(self.sock, "libdist_python_client", "init", pyerl.mk_list([]))
    
    def new(self, start, repl_method, repl_args, nodes):
        start = pyerl.mk_int(start)
        repl_method = pyerl.mk_atom(repl_method)
        repl_args = pyerl.mk_list([])
        nodes = [pyerl.mk_atom(x) for x in nodes ]
        nodes = pyerl.mk_list(nodes)
        
        inner_tuple = pyerl.mk_tuple((repl_method, repl_args, nodes))
        arguments = pyerl.mk_list([start, inner_tuple, self.retry])
        conf = pyerl.rpc(self.sock, "libdist_python_client", "new", arguments)
        return conf
    
    def cmd(self, conf, cmd):
        conf = conf
        cmd = data_py2erl(cmd)
        arguments = pyerl.mk_list([conf, cmd, self.retry])
        result = pyerl.rpc(self.sock, "libdist_python_client", "do", arguments)
        return result

    def reconfigure(self, conf, replica, newargs):
        conf = conf
        replica = pyerl.mk_string(replica)
        newargs = pyerl.mk_string(newargs)
        arguments = pyerl.mk_list([conf, replica, newargs, self.retry])
        result = pyerl.rpc(self.sock, "libdist_python_client", "reconfigure", arguments)
        return result

    def fork(self, conf, n, node, arg_list):
        conf = conf
        n = pyerl.mk_int(n)
        node = pyerl.mk_string(node)
        arguments = pyerl.mk_list([conf, n, node, arg_list])
        result = pyerl.rpc(self.sock, "libdist_python_client", "fork", arguments)
        return result

    def stop(self):
        conf = conf
        arguments = pyerl.mk_list([conf, pyerl.mk_int(n), pyerl.mk_string(node), self.retry])
        result = pyerl.rpc(self.sock, "libdist_python_client", "stop", arguments)
        return int(result)


        
        