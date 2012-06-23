#include <stdio.h>
#include "calc.h"

#define RETRY 1000

#define ADD(confId, value)   do_cmd(confId, 'a', value)
#define MULT(confId, value)  do_cmd(confId, 'm', value)
#define WRITE(confId, value) do_cmd(confId, 'w', value)
#define READ(confId)         do_cmd(confId, 'r', 0)

int do_cmd(long confId, char op, int value) {
   int ret;
   int *result;
   Command cmd = {op, value};

   libdist_cmd(confId, &cmd, sizeof(cmd), RETRY, &result);
   ret = *result;
   free(result);

   return ret;
}

int main() {
   int result, startingVal, repArgs;
   long confId = 0;
   char *nodes[] = {"node1@127.0.0.1", "node2@127.0.0.1", "node3@127.0.0.1"};

   libdist_init("node4@127.0.0.1", "libdist");

   startingVal = 1;
   repArgs = -1;  // XXX: not implemented yet
   confId = libdist_new(&startingVal, sizeof(startingVal),
         "chain", &repArgs, sizeof(repArgs), nodes, 3, RETRY);

   result = READ(confId);
   printf("Starting value is = %d\n", result);

   result = ADD(confId, 10);
   printf("Added 10, result is = %d\n", result);

   result = MULT(confId, 11);
   printf("Multiplied by 11, result is = %d\n", result);

   result = WRITE(confId, 50);
   printf("Wrote 50, result is = %d\n", result);

   return 0;
}

