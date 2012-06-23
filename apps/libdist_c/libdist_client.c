#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "ei.h"
#include "libdist_client.h"

#define ERR(a) fprintf(stderr, "Error: %s\n", a)

#define LOCAL  "ld_cnode"
#define MODULE "libdist_c_client"
#define INIT   "init"
#define NEW    "new"
#define DO     "do"
#define RECONF "reconfigure"
#define FORK   "fork"
#define STOP   "stop"


static int libdist_proxy_fd;
static ei_cnode libdist_ec;


int libdist_init(char* proxy_name, const char *cookie) {
   ei_x_buff args, response;

   // initialize local CNode
   if (ei_connect_init(&libdist_ec, LOCAL, cookie, 0) < 0) {
      ERR("Could not initialize local cnode");
      return -1;
   }

   // Connect to proxy Erlang node
   if ( (libdist_proxy_fd = ei_connect(&libdist_ec, proxy_name)) < 0 ) {
      ERR("Could not connect to proxy node");
      return -1;//FIXME: should be erl_errno but cannot compile for some reason;
   }

   ei_x_new(&args);
   ei_x_new(&response);
   ei_x_encode_empty_list(&args);

   if (ei_rpc(&libdist_ec, libdist_proxy_fd, MODULE, INIT,
            (const char *) (args.buff), args.index, &response) < 0) {
      ERR("Could not initialize proxy process");
      return -1;
   } else {
      ei_x_free(&args);
      ei_x_free(&response);
      return 0;
   }
}


long libdist_new(const void *coreArgs, long coreArgsLen,
      const char *repProtocol, void *repArgs, int argsLen,
      const char **nodes, int nodesLen,
      int retry) {

   int i, v;
   long conf;
   ei_x_buff args, result;

   ei_x_new(&args);
   ei_x_encode_list_header(&args, 3);
   ei_x_encode_binary(&args, coreArgs, coreArgsLen);
   ei_x_encode_tuple_header(&args, 3);
   ei_x_encode_atom(&args, repProtocol);
   ei_x_encode_empty_list(&args);   // XXX: ignoring protocol args for now
   ei_x_encode_list_header(&args, nodesLen);
   for (i=0; i<nodesLen; i++)
      ei_x_encode_atom(&args, nodes[i]);
   ei_x_encode_empty_list(&args);
   ei_x_encode_long(&args, retry);
   ei_x_encode_empty_list(&args);

   ei_x_new(&result);
   if (ei_rpc(&libdist_ec, libdist_proxy_fd, MODULE, NEW,
         (const char *) (args.buff), args.index, &result) < 0) {

      ei_x_free(&args);
      return -1;//FIXME: should be erl_errno but cannot compile for some reason;
   } else {
      i = 0;
      ei_decode_long((const char *) (result.buff), &i, &conf);
      ei_x_free(&args);
      ei_x_free(&result);
      return conf;
   }
}


long libdist_cmd(long conf, const void *command, long cmdLen, int retry, 
      void **resBuf) {

   int tmp, iLen, i = 0;
   long resLen;
   ei_x_buff args, result;

   ei_x_new(&args);
   ei_x_encode_list_header(&args, 3);
   ei_x_encode_long(&args, conf);
   ei_x_encode_binary(&args, command, cmdLen);
   ei_x_encode_long(&args, retry);
   ei_x_encode_empty_list(&args);

   ei_x_new(&result);
   if (ei_rpc(&libdist_ec, libdist_proxy_fd, MODULE, DO,
            (const char *) (args.buff), args.index, &result) < 0) {

      ei_x_free(&args);
      return -1;//FIXME: should be erl_errno but cannot compile for some reason;
   } else {
      ei_get_type((const char *) result.buff, &i, &tmp, &iLen);
      *resBuf = malloc(iLen);
      ei_decode_binary((const char *) result.buff, &i, *resBuf, &resLen);
      ei_x_free(&args);
      ei_x_free(&result);
      return resLen;
   }
}


long libdist_reconfigure(long conf, void **newReplicas, int numReplicas,
      void **newArgs, int numArgs, int retry) {

   int i;
   long newConf;
   ei_x_buff args, result;

   ei_x_new(&args);
   ei_x_encode_list_header(&args, 4);
   ei_x_encode_long(&args, conf);
   ei_x_encode_list_header(&args, numReplicas);
   for (i=0; i<numReplicas; i++)
      ei_x_encode_pid(&args, (erlang_pid *) newReplicas[i]);
   ei_x_encode_empty_list(&args);
   ei_x_encode_empty_list(&args);   // XXX: ignore rep protocol args for now
   ei_x_encode_long(&args, retry);
   ei_x_encode_empty_list(&args);

   ei_x_new(&result);
   if (ei_rpc(&libdist_ec, libdist_proxy_fd, MODULE, RECONF,
            (const char *) (args.buff), args.index, &result) < 0) {

      ei_x_free(&args);
      return -1;//FIXME: should be erl_errno but cannot compile for some reason;
   } else {
      i = 0;
      ei_decode_long((const char *) (result.buff), &i, &newConf);
      ei_x_free(&args);
      ei_x_free(&result);
      return newConf;
   }
}


int libdist_fork(long conf, int n, const char *node, const void *forkArgs,
      int len, void *forked_pid) {

   ei_x_buff args, result;

   ei_x_new(&args);
   ei_x_encode_list_header(&args, 4);
   ei_x_encode_long(&args, conf);
   ei_x_encode_long(&args, n);
   ei_x_encode_atom(&args, node);
   ei_x_encode_binary(&args, forkArgs, len);
   ei_x_encode_empty_list(&args);

   ei_x_new(&result);
   if (ei_rpc(&libdist_ec, libdist_proxy_fd, MODULE, FORK,
            (const char *) (args.buff), args.index, &result) < 0) {

      ei_x_free(&args);
      return -1;//FIXME: should be erl_errno but cannot compile for some reason;
   } else {
      ei_decode_pid((const char*) (result.buff), &(result.buffsz),
            (erlang_pid *) forked_pid);
      ei_x_free(&args);
      ei_x_free(&result);
      return 0;
   }
}
