-module(libdist_python_client).
-export([
      init/0,
      new/3,
      do/3,
      fork/4,
      reconfigure/4,
      stop/4
   ]).

-include_lib("repobj/include/repobj.hrl").

% Initialize a client proxy
init() ->
   case whereis(proxy) of
      undefined -> register(proxy, spawn(fun() -> loop(1) end));
      _ -> ok
   end.

% Create a new replicated object
new(CoreArgs, {RepProtocol, RepArgs, Nodes}, Retry) ->
   Conf = RepProtocol:new({libdist_python_server, CoreArgs}, RepArgs, Nodes, Retry),
   proxy ! {self(), reg_conf, Conf},
   receive ConfId -> ConfId end.

% Execute a command synchronously on a replicated object
do(ConfId, Command, Retry) ->
   Conf = #conf{protocol = Module} = get_conf(ConfId),
   Module:do(Conf, Command, Retry).

% Reconfigure a replicated object using the new replicas and replication
% arguments
reconfigure(ConfId, NewReplicas, NewArgs, Retry) ->
   Conf = #conf{protocol = Module} = get_conf(ConfId),
   Module:reconfigure(Conf, NewReplicas, NewArgs, Retry).

% Fork a copy of Nth replica in the configuration on the specified node and
% given the provided arguments
fork(ConfId, N, Node, Args) ->
   Conf = #conf{protocol = Module} = get_conf(ConfId),
   Module:fork(Conf, N, Node, Args).

% Stop the Nth replica of the given configuration with a given reason
stop(ConfId, N, Reason, Retry) ->
   Conf = #conf{protocol = Module} = get_conf(ConfId),
   Module:stop(Conf, N, Reason, Retry).


%%%%%%%%%%%%%%%%%%%%%
% Private Functions %
%%%%%%%%%%%%%%%%%%%%%

loop(I) ->
   receive
      {Client, reg_conf, Conf} ->
         put(I, Conf),
         Client ! I,
         loop(I+1);

      {Client, get_conf, ConfId} ->
         Conf = get(ConfId),
         Client ! {ok, Conf},
         loop(I)
   end.


get_conf(ConfId) ->
   proxy ! {self(), get_conf, ConfId},
   receive
      {ok, Conf} -> Conf
      % TODO: do something sensible if configuration was not found
   end.
