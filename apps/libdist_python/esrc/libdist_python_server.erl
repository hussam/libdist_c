-module(libdist_python_server).
-export([ 
      new/1,
      do/2,
      fork/3,
      is_mutating/1,
      stop/2
   ]).

new(Value) ->
   io:format("In new() as ~p", [self()]),
   {ok, SoName} = application:get_env(libdist_python, server_lib),
   Script = string:concat("python -u ", SoName),
   Port = open_port({spawn, Script},
           [{packet, 1}, binary, use_stdio]),
   % Convert tuple {hello, Name} to external term format
   % InitData = term_to_binary({init, nil}),
   % Send binary data to hello.py script
   % port_command(Port, InitData),
   
   put(python_port, Port),

   NewData = term_to_binary({new, Value}),

   port_command(Port, NewData),
   
   % Wait for reply from hello.py script
   receive
       {Port, {data, RespData}} ->
           % Convert binary data to term
           binary_to_term(RespData)
   after
       5000 ->
           {error, timeout}
   end.

do(InstanceId, Command) ->
	io:format("In do() as ~p", [self()]),
    Port = get(python_port),
    % Convert tuple {hello, Name} to external term format
    ReqData = term_to_binary({cmd, InstanceId, Command}),
    % Send binary data to hello.py script
    port_command(Port, ReqData),
    % Wait for reply from hello.py script
    receive
        {Port, {data, RespData}} ->
            % Convert binary data to term
            binary_to_term(RespData)
    after
        1000 ->
            {error, timeout}
    end.


fork(_InstanceId, ForkNode, ForkArgs) ->
	io:format("In fork() as ~p", [self()]),
    Port = get(python_port),
    % Convert tuple {hello, Name} to external term format
    ReqData = term_to_binary({fork, ForkNode, ForkArgs}),
    % Send binary data to hello.py script
    port_command(Port, ReqData),
    % Wait for reply from hello.py script
    receive
        {Port, {data, RespData}} ->
            % Convert binary data to term
            binary_to_term(RespData)
    after
        5000 ->
            {error, timeout}
    end.


is_mutating(Command) ->
	io:format("In is_mutating() as ~p", [self()]),
    {ok, SoName} = application:get_env(libdist_python, server_lib),
    Script = string:concat("python -u ", SoName),
    Port = open_port({spawn, Script},
            [{packet, 1}, binary, use_stdio]),

    % Convert tuple {hello, Name} to external term format
    ReqData = term_to_binary({is_mutating, Command}),
    % Send binary data to hello.py script
    port_command(Port, ReqData),
    % Wait for reply from hello.py script
    receive
        {Port, {data, RespData}} ->
            % Convert binary data to term
            binary_to_term(RespData)
    after
        5000 ->
            {error, timeout}
    end.


stop(_InstanceId, Reason) ->
	io:format("in stop() as ~p", [self()]),
    Port = get(python_port),
    % Convert tuple {hello, Name} to external term format
    ReqData = term_to_binary({stop, Reason}),
    % Send binary data to hello.py script
    port_command(Port, ReqData),
    % Wait for reply from hello.py script
    receive
        {Port, {data, RespData}} ->
            % Convert binary data to term
			port_close(Port),
            binary_to_term(RespData)
    after
        5000 ->
            {error, timeout}
    end.
