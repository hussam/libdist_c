-module(echo).

% Echo Public API
-export([
      start/3,
      start_chain/1,
      start_pb/1,
      echo/2,
      set_tag/2,
      fork_replica/3,
      stop_replica/3
   ]).

% RepObj Interface Implementation
-export([
      new/1,
      do/2,
      fork/3,
      is_mutating/1,
      stop/2
   ]).


% 5 second timeout
-define(TIMEOUT, 5000).


% Start a replicated 'echo' server given using RepProtocol on a bunch of Nodes
start(RepProtocol, RepArgs, Nodes) ->
   % Create a new replicated object.
   % The function 'repobj:new/3' takes 3 parameters, the first is a tuple
   % specifying the core module's name and list of arguments to pass to its
   % new/1 function. The second is a tuple specifying the replication protocol,
   % arguments to the replication protocol (if any), and a list of nodes to
   % deploy the replicated object on. The last parameter is a measure for when
   % to timeout on trying to create the object if things don't go right.
   repobj:new({?MODULE, [echo_server]}, {RepProtocol, RepArgs, Nodes}, ?TIMEOUT).

% Starts a chain replicated 'echo' server on a bunch of nodes
start_chain(Nodes) ->
   repobj:new({?MODULE, [echo_server]}, {chain, [], Nodes}, ?TIMEOUT).

% Starts a primary/backup replicated 'echo' server on a bunch of nodes
start_pb(Nodes) ->
   repobj:new({?MODULE, [echo_server]}, {primary_backup, [], Nodes}, ?TIMEOUT).

% Send a message to be echoed by the replicated object
echo(Obj, Message) ->
   repobj:do(Obj, {echo, Message}, ?TIMEOUT).

% Updated the tag used by the echo server
set_tag(Obj, NewTag) ->
   repobj:do(Obj, {set_tag, NewTag}, ?TIMEOUT).

% Fork the nth replica of a replicated object
fork_replica(Obj, N, ForkNode) ->
   Ref = repobj:fork(Obj, N, ForkNode, []),
   receive
      {Ref, ForkedPid} -> ForkedPid
   end.

% Stop the nth replica of a replicated object
stop_replica(Obj, N, Reason) ->
   repobj:stop(Obj, N, Reason, ?TIMEOUT).



% All the functions below are just implementing the repobj interface



% The new/1 function takes in a bunch of arguments and returns the id of a new
% local instance of the core. In this case, the arguments contain a tag used by
% the echo server.
new(_Args = [Tag]) ->
   put(tag, Tag),
   self().

% The do/2 function takes in an instance Id, and a command and executes the
% command. The echo server understands two types of commands, 'set_tag' which
% modifies the internal state of the server, and 'echo' which is a 'readonly'
% command that echoes the passed in message.
do(Pid, {set_tag, NewTag}) ->
   put(tag, NewTag),
   {Pid, ok};
do(Pid, {echo, Message}) ->
   {Pid, get(tag), Message};
do(_, _) ->
   undefined_op.

% The fork/3 function takes in an instance Id, a node name, and arguments and
% creates a copy of "instance Id" on the supplied node, according to the
% arguments specified. In this implementation, all it does is spawn a new echo
% server on the given node.
fork(_Pid, ForkNode, _ForkArgs) ->
   spawn(ForkNode, ?MODULE, new, [[get(tag)]]).

% The is_mutating/1 function takes in a command and returns true/false if the
% command will change the internal state of the local core or not.
is_mutating({set_tag, _}) ->
   true;
is_mutating({echo, _}) ->
   false.

% The stop/2 function stops the specified instance given the reason passed to it
% by the repobj. This could be due to reconfiguration or a specific user
% request.
stop(Pid, Reason) ->
   io:format("Stopping ~p because ~p\n", [Pid, Reason]).
