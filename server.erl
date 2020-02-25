-module(server).

-export([start/1, stop/1]).

-record(server_state, {clients, channels}).
-record(channel_state, {name, clients}).
-record(client, {pid, nick}).

server_state() -> #server_state{clients = [], channels = []}.

channel_state(Name) -> #channel_state{name = Name, clients = []}.

client(Pid, Nick) -> #client{pid = Pid, nick = Nick}.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  % TODO Implement function
  % - Spawn a new process which waits for a message, handles it, then loops infinitely
  % - Register this process to ServerAtom
  % - Return the process ID
  catch genserver:start(ServerAtom, server_state(), fun handle/2).

nick_exists(Nick, State) -> true.

pid_exists(Nick, State) -> false.

channel_exists(Channel, State) -> true.

% returns { Result, NewState }
% if the Pid is not available, it means that the user is already connected
handle_join(State, RequestInput = {_, ClientPid, _}) -> 
  handle_join_check_pid_exists(State, pid_exists(ClientPid, State), RequestInput).

handle_join_check_pid_exists(State, true, RequestInput = {ChannelName, _, _}) ->
  handle_join_check_channel_exists(State, channel_exists(ChannelName, State), RequestInput);

handle_join_check_pid_exists(State, false, RequestInput = {_, _, ClientNick}) ->
  handle_join_check_nick_exists(State, nick_exists(ClientNick, State), RequestInput).

handle_join_check_nick_exists(State, true, _) ->
  {nick_taken, State};

handle_join_check_nick_exists(State, false, RequestInput = {ChannelName, ClientPid, ClientNick}) ->
  handle_join_check_channel_exists(State#server_state{clients = [client(ClientPid, ClientNick) | State#server_state.clients]}, channel_exists(ChannelName, State), RequestInput).

handle_join_check_channel_exists(State, true, RequestInput = {ChannelName, _, _}) ->
  genserver:start(
    list_to_atom(ChannelName),
    channel_state(ChannelName),
    fun channel/2
  ),
  handle_join_join_channel(State#server_state{channels = [ChannelName | State#server_state.channels]}, RequestInput);

handle_join_check_channel_exists(State, false, RequestInput) ->
  handle_join_join_channel(State, RequestInput).

handle_join_join_channel(State, {ChannelName, ClientPid, _}) ->
  { genserver:request(list_to_atom(ChannelName), {join, ClientPid}), State }.

handle(State, {join, ChannelName, ClientPid, ClientNick}) ->
  {Result, NewState} = handle_join(
    State,
    {ChannelName, ClientPid, ClientNick}
  ),
  {reply, Result, NewState};
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
    true ->
      {reply, ok, State#channel_state{clients = lists:delete(Client, State#channel_state.clients)}};
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
                      genserver:request(
                        Pid,
                        {message_receive, State#channel_state.name, Nick, Message}
                      )
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
