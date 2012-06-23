#include "erl_nif.h"
#include "libdist_server.h"

void* libdist_alloc_bin(size_t size, void **ebin)
{
   ErlNifBinary *bin = malloc(sizeof(ErlNifBinary));
   enif_alloc_binary(sizeof(int), bin);
   *ebin = bin;
   return bin->data;
}

static ERL_NIF_TERM new_nif ( ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[] )
{
   void *state;
   size_t stateLen;
   ErlNifBinary args, instanceState;

   if ( enif_inspect_binary(env, argv[0], &args) ) {
      // TODO: handle initialization errors (when new(...) < 0)
      new((void *) args.data, args.size, &state, &stateLen);
      enif_alloc_binary(stateLen, &instanceState);
      memcpy(instanceState.data, state, stateLen);
      free(state);
      return enif_make_binary(env, &instanceState);
   } else {
      return enif_make_badarg(env);
   }
}

static ERL_NIF_TERM cmd_nif ( ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[] )
{
   ErlNifBinary instance, command, *result;
   if (  enif_inspect_binary(env, argv[0], &instance) &&
         enif_inspect_binary(env, argv[1], &command) )
   {
      result = (ErlNifBinary *) cmd(instance.data, command.data, command.size);
      return enif_make_binary(env, result);
   }
   else
   {
      return enif_make_badarg(env);
   }
}

static ERL_NIF_TERM is_mutating_nif ( ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[] )
{
   ErlNifBinary command;
   if ( enif_inspect_binary(env, argv[0], &command) )
   {
      if (is_mutating(command.data, command.size))
         return enif_make_atom(env, "true");
      else
         return enif_make_atom(env, "false");
   }
   else
   {
      return enif_make_badarg(env);
   }
}

static ERL_NIF_TERM fork_nif ( ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[] )
{
   char *node;
   int nodeLen;

   void *forkedState = malloc(sizeof(void *));
   size_t forkedStateLen;

   ErlNifBinary instance, args, forkedInstance;

   if ( enif_get_atom_length(env, argv[1], &nodeLen, ERL_NIF_LATIN1) )
   {
      node = (char *) malloc(nodeLen + 1);
      if (  enif_inspect_binary(env, argv[0], &instance) &&
            enif_get_atom(env, argv[1], node, nodeLen, ERL_NIF_LATIN1) &&
            enif_inspect_binary(env, argv[2], &args) )
      {
         // TODO: handle fork errors (when fork(...) < 0)
         fork_replica(instance.data, node, args.data, args.size, &forkedState,
               &forkedStateLen);
         enif_alloc_binary(forkedStateLen, &forkedInstance);
         memcpy(forkedInstance.data, forkedState, forkedStateLen);
         free(forkedState);
         return enif_make_binary(env, &forkedInstance);
      }
      else
      {
         enif_make_badarg(env);
      }

   }
   else
   {
      return enif_make_badarg(env);
   }
}

static ERL_NIF_TERM stop_nif ( ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[] )
{
   ErlNifBinary instance, reason, *result;
   if (  enif_inspect_binary(env, argv[0], &instance) &&
         enif_inspect_binary(env, argv[1], &reason) )
   {
      result = (ErlNifBinary *) stop(instance.data, reason.data, reason.size);
      return enif_make_binary(env, result);
   }
   else
   {
      return enif_make_badarg(env);
   }
}

static ErlNifFunc libdist_nif_funcs[] = {
   {"new",         1, new_nif},
   {"is_mutating", 1, is_mutating_nif},
   {"do",          2, cmd_nif},
   {"fork",        3, fork_nif},
   {"stop",        2, stop_nif}
};

ERL_NIF_INIT(libdist_c_server, libdist_nif_funcs, NULL, NULL, NULL, NULL);
