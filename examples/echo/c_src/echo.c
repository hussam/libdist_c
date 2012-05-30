#include <string.h>
#include <stdio.h>
#include "erl_nif.h"

ERL_NIF_TERM tag, undefined_op, ok, ctag;

static int load (ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
   tag          = enif_make_atom(env, "c_echo_server");
   undefined_op = enif_make_atom(env, "undefined_op");
   ok           = enif_make_atom(env, "ok");
   ctag         = enif_make_atom(env, "c_driver");
   return 0;
}

static ERL_NIF_TERM do_nif( ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[] )
{
   char cmd[8];
   int tupleLen;
   ERL_NIF_TERM *pid, *cmdTuple;

   // Expect 2 arguments. The first should be a local process id, the second
   // should be a tuple of {command, Argument}. 'command' should be an atom.
   // Check that these constraints are met, read the corresponding values, and
   // return 'undefined_op' if any of those conditions are not met.
   if ( !enif_get_tuple(env, argv[1], &tupleLen, (const ERL_NIF_TERM **) &cmdTuple) ||
         tupleLen != 2 ||
         !enif_get_atom(env, cmdTuple[0], cmd, 8, ERL_NIF_LATIN1) )
   {
      return undefined_op;
   }
    // the first argument is just the local process id
   pid = (ERL_NIF_TERM *) argv;

   if ( strcmp(cmd, "set_tag") == 0 )
   {
      tag = cmdTuple[1];
      return enif_make_tuple(env, 3, *pid, ctag, ok);
   }
   else if ( strcmp(cmd, "echo") == 0 )
   {
      return enif_make_tuple(env, 4, *pid, ctag, tag, cmdTuple[1]);
   }
   else
   {
      return undefined_op;
   }
}

static ErlNifFunc echo_nif_funcs[] = {
   {"do", 2, do_nif}
};

ERL_NIF_INIT(echo, echo_nif_funcs, &load, NULL, NULL, NULL)
