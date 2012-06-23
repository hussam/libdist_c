#ifndef _LIBDIST_SERVER_H
#define _LIBDIST_SERVER_H

#include <string.h>
#include <stdlib.h>

void* libdist_alloc_bin(size_t size, void **ebin);


// Server-defined functions

/* Initialize a new server */
int new (void *args, size_t argsLen, void **state, size_t *stateLen);

/* Execute a command */
void* cmd (void *instance, void *cmd, size_t cmdLen);

/* Is the command mutating or not? */
int is_mutating (void *cmd, size_t cmdLen);

/* Create a new server that is a replica of the given server */
int fork_replica (  void *localState,
                           char *forkNode, void *forkArgs, size_t argsLen,
                           void **forkedState, size_t *stateLen);

/* Stop a given server for a given reason */
void* stop (void *state, void *reason, size_t reasonLen);

#endif
