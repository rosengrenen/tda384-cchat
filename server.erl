-module(server).

-export([start/1, stop/1]).

-record(
  server_state,
  {
    clients,
    channels
  }
).

-record(
  channel_state,
  {
    name,
    clients
  }
).

-record(
  client,
  {
    pid,
    nick
  }
).

server_state() -> #server_state{clients = [], channels = []}.
channel_state(Name, Clients) -> #channel_state{name = Name, clients = Clients}.
client(Pid, Nick) -> #client{pid = Pid, nick = Nick}.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  % TODO Implement function
  % - Spawn a new process which waits for a message, handles it, then loops infinitely
  % - Register this process to ServerAtom
  % - Return the process ID
  catch genserver:start(ServerAtom, [], fun handle/2).

handle(State, {join, Channel, Client}) ->
  case lists:member(Channel, State) of
    true ->
      Result = genserver:request(list_to_atom(Channel), {join, Client}),
      {reply, Result, State};
    false ->
      genserver:start(list_to_atom(Channel), initial_state(Channel, [Client]), fun channel/2),
      {reply, ok, [Channel | State]}
  end;
handle(State, {exit, _}) -> 
  lists:foreach(fun (Channel) -> genserver:stop(list_to_atom(Channel)) end, State),
  {reply, ok, State}.

channel(State, {join, Client}) ->
  case lists:member(Client, State#channel_state.clients) of
    true -> {reply, user_already_joined, State};
    false -> {reply, ok, State#channel_state{clients = [Client | State#channel_state.clients]}}
  end;
channel(State, {leave, Client}) ->
  case lists:member(Client, State#channel_state.clients) of
    true -> {reply, ok, State#channel_state{clients = lists:delete(Client, State#channel_state.clients)}};
    false -> {reply, user_not_joined, State}
  end;
channel(State, {message_send, Client, Nick, Message}) ->
  case lists:member(Client, State#channel_state.clients) of
    true ->
      spawn(
        fun
          () ->
            lists:foreach(
              fun
                (Pid) ->
                  if
                    Pid == Client -> skip;
                    true ->
                      genserver:request(Pid, {message_receive, State#channel_state.name, Nick, Message})
                  end
              end,
              State#channel_state.clients
            )
        end
      ),
      {reply, ok, State};
    false -> {reply, user_not_joined, State}
  end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
  % TODO Implement function
  % Return ok
  genserver:request(ServerAtom, {exit, {}}),
  genserver:stop(ServerAtom),
  ok.
