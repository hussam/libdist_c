-module(libdist_c_server).
-export([
      load_nif/1,
      new/1,
      do/2,
      fork/3,
      is_mutating/1,
      stop/2
   ]).

load_nif(SoName) ->
   erlang:load_nif(SoName, 0).

new(_Args) ->
   exit(nif_library_not_loaded).

do(_InstanceId, _Command) ->
   exit(nif_library_not_loaded).

fork(_InstanceId, _ForkNode, _ForkArgs) ->
   exit(nif_library_not_loaded).

is_mutating(_Command) ->
   exit(nif_library_not_loaded).

stop(_InstanceId, _Reason) ->
   exit(nif_library_not_loaded).
