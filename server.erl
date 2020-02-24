-module(server).

-export([start/1, stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    catch genserver:start(ServerAtom, [], fun handle/2).

handle(State, {join, Channel, Client}) ->
    io:fwrite("~p~n~p~n", [State, {join, Channel, Client}]),
    case lists:member(Channel, State) of
      true ->
	  Result = genserver:request(list_to_atom(Channel),
				     {join, Client}),
	  {reply, Result, State};
      false ->
	  genserver:start(list_to_atom(Channel), [Client],
			  fun channel/2),
	  {reply, ok, [Channel | State]}
    end;
handle(State, {leave, Channel, Client}) ->
    case lists:member(Channel, State) of
      true ->
	  Result = genserver:request(list_to_atom(Channel),
				     {leave, Client}),
	  {reply, Result, State};
      false -> {reply, invalid_channel, State}
    end.

channel(State, {join, Client}) ->
    io:fwrite("[Channel], ~p,~p~n", [State, Client]),
    case lists:member(Client, State) of
      true -> {reply, user_already_joined, State};
      false -> {reply, ok, [Client | State]}
    end;
channel(State, {leave, Client}) ->
    io:fwrite("[Channel], ~p,~p~n", [State, Client]),
    case lists:member(Client, State) of
      true -> {reply, ok, lists:delete(Client, State)};
      false -> {reply, user_not_joined, State}
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
