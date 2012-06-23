#ifndef _LIBDIST_CLIENT_H
#define _LIBDIST_CLIENT_H

/*
 * Initialize the client library.
 *
 * arguments:
 *    - proxy_name:  Name of a local proxy Erlang node to send/receive requests
 *    - cookie:      Cookie string shared with nodes in the cluster
 *
 * return: 0 if successful or a negative error code.
 */
int libdist_init(char* proxy_name, const char *cookie);


/*
 * Create a new distributed object.
 *
 * arguments:
 *    - coreArgs:       Arguments sent to initialize the server core
 *    - coreArgsLen:    Length of arguments to server core
 *    - repProtocol:    Name of replication protocol
 *    - repArgs:        Optional replication arguments (XXX: currently ignored)
 *    - repArgsLen:     Length of replication arguments
 *    - nodes:          List of nodes to host the distributed object
 *    - nodesLen:       Length of nodes list
 *
 * return - a new configuration number if successful or a negative error code
 */
long libdist_new(const void *coreArgs, long coreArgsLen,
      const char *repProtocol, void *repArgs, int argsLen,
      const char **nodes, int nodesLen,
      int retry);


/*
 * Execute a command on a distributed object with the given configuration.
 * Results will be written to the passed in buffer. The buffer will be
 * reallocated if result did not fit. If so, bufLen will be adjusted
 * accordingly.
 *
 * arguments:
 *    - conf:     configuration ID of the distributed object
 *    - command:  the command to execute
 *    - cmdLen:   length of the command buffer
 *    - retry:    waiting period before retrying the command
 *    - buf:      a pointer to the result buffer
 *
 * return - length of the return buffer if successful or a negative error code
 */
long libdist_cmd(long conf, const void *command, long cmdLen, int retry, 
      void **resBuf);


/*
 * Reconfigure a distributed object.
 * Currently only supports changing the set of replicas comprising the object
 *
 * arguments:
 *    - conf:           current configuration ID of the distributed object
 *    - newReplicas:    the new set of replicas
 *    - newArgs:        the new replication arguments (XXX: currently ignored)
 *    - retry:          waiting period before retrying the reconfiguration
 *
 * return - a new configuration number if successful or a negative error code
 */
long libdist_reconfigure(long conf, void **newReplicas, int numReplicas,
      void **newArgs, int numArgs, int retry);


/*
 * Fork a replica of a given configuration.
 *
 * arguments:
 *    - conf:        configuration identifier of the distributed object
 *    - n:           replica number in the configuration
 *    - node:        node on which to place the forked replica
 *    - forkArgs:    arguments to the replica's fork function
 *    - len:         length of forkArgs
 *    - forked_pid:  the new forked replica's process identifier will be set
 *                   here
 *
 * return - 0 if successful or a negative error code.
 */
int libdist_fork(long conf, int n, const char *node, const void *forkArgs,
      int len, void *forked_pid);

#endif
