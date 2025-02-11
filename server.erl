-module(server).

-export([start/1, stop/1]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(
  channel_st,
  {
    % channel id
    id,
    clients
  }
).

%clients in the server
initial_state(Id, Clients) -> #channel_st{id = Id, clients = Clients}.

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
  case lists:member(Client, State#channel_st.clients) of
    true -> {reply, user_already_joined, State};
    false -> {reply, ok, State#channel_st{clients = [Client | State#channel_st.clients]}}
  end;
channel(State, {leave, Client}) ->
  case lists:member(Client, State#channel_st.clients) of
    true -> {reply, ok, State#channel_st{clients = lists:delete(Client, State#channel_st.clients)}};
    false -> {reply, user_not_joined, State}
  end;
channel(State, {message_send, Client, Nick, Message}) ->
  case lists:member(Client, State#channel_st.clients) of
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
                      genserver:request(Pid, {message_receive, State#channel_st.id, Nick, Message})
                  end
              end,
              State#channel_st.clients
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
