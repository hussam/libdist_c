#!/usr/bin/env python
import time

from libdist.client import LibDistClient

client = LibDistClient("python", "127.0.0.1", "node1", "libdist")

starting_value = 1
nodes = (u'node1@127.0.0.1', u'node2@127.0.0.1', u'node3@127.0.0.1', u'node4@127.0.0.1')
conf = client.new(starting_value, u'chain', [], nodes)

cmd1 = (u'r', -1)
result1 = client.cmd(conf, cmd1)
print "READ result is %s" % str(result1)

cmd2 = (u'a', 10)
result2 = client.cmd(conf, cmd2)
print "ADD result is %s" % str(result2)

cmd3 = (u'm', 11)
result3 = client.cmd(conf, cmd3)
print "MULTIPLY result is %s" % str(result3)

cmd4 = (u'w', 50)
result4 = client.cmd(conf, cmd4)
print "WRITE result is %s" % str(result4)
