A simple calculator application that uses `libdist_c` (the C wrapper around libdist).

To run this, first get libdist's dependencies and compile:

```sh
$ libdist > make
```

Next, generate development release nodes to that will host the distributed
system, and copy those nodes to the example's directory.

```sh
$ libdist > make devrel
$ libdist > cp -r dev examples/calc/
```

Go to the example's directory and compile. This will produce a server library
(`calc_server.so`) and a client application (`calc_client`). The Makefile copies
the `libdist` client and server libraries to the local directory, and it uses
them to create a `calc` client and a server library. Copy the `calc` server
library and `libdist_c` library to all the dev nodes.

```sh
$ libdist > cd examples/calc/
$ libdist/examples/calc > make
$ libdist/examples/calc > for d in dev/dev*; do cp -v libdist_server.so calc_server.so $d/; done
```

Now, use the `start` script in dev/tools to start the four nodes and pass to
`libdist_c` on all of them the name of the name of the server library to load.

```sh
$ libdist/examples/calc > ./dev/tools/start 1 4 -libdist_c server_lib calc_server
```

*NOTE:* If you are using OS X and have a firewall enabled, you might see
messages popping on the screen. That is just the firewall detecting that the
nodes are listening for connections.

The nodes are now up. You can ping them to make sure if you want.

```sh
$ libdist/examples/calc > ./dev/tools/ping 1 4
```

Now, you can run the client.

```sh
$ libdist/examples/calc > ./calc_client
Starting value is = 1
Added 10, result is = 11
Multiplied by 11, result is = 121
Wrote 50, result is = 50
```

To stop the nodes, use the included `stop` script.

```sh
$ libdist/examples/calc > ./dev/tools/stop 1 4
```

That's it!
