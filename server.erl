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

nick_exists(_, []) -> false;
nick_exists(Nick, [Client | RestOfClients]) ->
  case Nick == Client#client.nick of
    true -> true;
    false -> nick_exists(Nick, RestOfClients)
  end.

pid_exists(_, []) -> false;
pid_exists(Pid, [Client | RestOfClients]) ->
  case Pid == Client#client.pid of
    true -> true;
    false -> nick_exists(Pid, RestOfClients)
  end.

channel_exists(Channel, Channels) -> lists:member(Channel, Channels).

update_client_nick(_, _, []) -> [];
update_client_nick(NewNick, ClientPid, [Client | RestOfClients]) ->
  if
    ClientPid == Client#client.pid -> [Client#client{nick = NewNick} | RestOfClients];
    true -> [Client | update_client_nick(NewNick, ClientPid, RestOfClients)]
  end.

validate_client(State, RequestInput = {_, ClientPid, ClientNick}) ->
  case pid_exists(ClientPid, State#server_state.clients) of
    true -> check_channel(State, RequestInput);
    false ->
      case nick_exists(ClientNick, State#server_state.clients) of
        true -> {nick_taken, State};
        false ->
          check_channel(
            State#server_state{
              clients = [client(ClientPid, ClientNick) | State#server_state.clients]
            },
            RequestInput
          )
      end
  end.

check_channel(State, RequestInput = {ChannelName, _, _}) ->
  case channel_exists(ChannelName, State#server_state.channels) of
    true -> join_channel(State, RequestInput);
    false ->
      genserver:start(list_to_atom(ChannelName), channel_state(ChannelName), fun channel/2),
      join_channel(
        State#server_state{channels = [ChannelName | State#server_state.channels]},
        RequestInput
      )
  end.

join_channel(State, {ChannelName, ClientPid, _}) ->
  {genserver:request(list_to_atom(ChannelName), {join, ClientPid}), State}.

handle(State, {join, ChannelName, ClientPid, ClientNick}) ->
  {Result, NewState} = validate_client(State, {ChannelName, ClientPid, ClientNick}),
  {reply, Result, NewState};
handle(State, {nick, ClientPid, NewClientNick}) ->
  case lists:member(client(ClientPid, NewClientNick), State#server_state.clients) of
    true -> {reply, ok, State};
    false ->
      case nick_exists(NewClientNick, State#server_state.clients) of
        true -> {reply, nick_taken, State};
        false ->
          {
            reply,
            ok,
            State#server_state{
              clients = update_client_nick(NewClientNick, ClientPid, State#server_state.clients)
            }
          }
      end
  end;
handle(State, {exit, _}) ->
  lists:foreach(
    fun (Channel) -> genserver:stop(list_to_atom(Channel)) end,
    State#server_state.channels
  ),
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
