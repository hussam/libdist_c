#include <stdio.h>
#include "calc.h"
#include "libdist_server.h"


int new (void *args, size_t argsLen, void **state, size_t *stateLen)
{
   int *localValue;
   *stateLen = sizeof(int);
   localValue = (int *) malloc(*stateLen);
   *localValue = *(int *)args;
   *state = localValue;
   return 0;
}

void* cmd (void *state, void *buf, size_t bufLen)
{
   void *bin;
   int *result;
   int *localValue = (int *) state;
   Command *command = (Command *) buf;

   result = (int *) libdist_alloc_bin(sizeof(int), &bin);
   switch(command->op)
   {
      case 'a':   // ADD
         *result = *localValue += command->value;
         break;

      case 'm':   // MULTIPLY
         *result = *localValue *= command->value;
         break;

      case 'w':   // WRITE
         *localValue = command->value;
      case 'r':   // READ
         *result = *localValue;
         break;

      default:
         *result = -1;
         break;
   }
   return bin;
}

int is_mutating (void *cmd, size_t cmdLen)
{
   switch( ((Command *) cmd)->op )
   {
      case 'a':
      case 'm':
      case 'w':
         return 1;

      case 'r':
      default:
         return 0;
   }
}

int fork_replica (void *localState,
      char *forkNode, void *forkArgs, size_t argsLen,
      void **forkedState, size_t *stateLen)
{
   return new(localState, -1, forkedState, stateLen);
}

void* stop (void *state, void *reason, size_t reasonLen)
{
   void *bin;
   int *result = (int *) libdist_alloc_bin(sizeof(int), &bin);
   *result = 0;
   return bin;
}


