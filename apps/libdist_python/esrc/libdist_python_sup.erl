-module(libdist_python_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
   % Load the server NIF
   % case application:get_env(server_lib) of
   %    {ok, SoName} when is_atom(SoName) ->
   %       libdist_python_server:prep_port(atom_to_list(SoName));
   %    {ok, SoName} when is_list(SoName) ->
   %       libdist_python_server:prep_port(SoName);
   %    undefined ->
   %       do_nothing
   % end,
   {ok, { {one_for_one, 5, 10}, []} }.

