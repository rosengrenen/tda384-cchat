-module(server).

-export([start/1, stop/1]).

-record(server_state, {clients, channels}).
-record(channel_state, {name, clients}).
-record(client, {pid, nick}).

% Create new server state
server_state() -> #server_state{clients = [], channels = []}.

% Create new channel state
channel_state(Name) -> #channel_state{name = Name, clients = []}.

% Create new client record
client(Pid, Nick) -> #client{pid = Pid, nick = Nick}.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  % TODO Implement function
  % - Spawn a new process which waits for a message, handles it, then loops infinitely
  % - Register this process to ServerAtom
  % - Return the process ID
  catch genserver:start(ServerAtom, server_state(), fun handle/2).

% Handle a join request
handle(State, {join, ChannelName, ClientPid, ClientNick}) ->
  {Result, NewState} = validate_client(State, {ChannelName, ClientPid, ClientNick}),
  {reply, Result, NewState};
% Handle a nick change request
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
% Handle a server shut down request
handle(State, {exit, _}) ->
  lists:foreach(
    fun (Channel) -> genserver:stop(list_to_atom(Channel)) end,
    State#server_state.channels
  ),
  {reply, ok, State}.

% Handle a channel join request
channel(State, {join, ClientPid}) ->
  case lists:member(ClientPid, State#channel_state.clients) of
    true -> {reply, user_already_joined, State};
    false -> {reply, ok, State#channel_state{clients = [ClientPid | State#channel_state.clients]}}
  end;
% Handle a channel leave request
channel(State, {leave, ClientPid}) ->
  case lists:member(ClientPid, State#channel_state.clients) of
    true ->
      {
        reply,
        ok,
        State#channel_state{clients = lists:delete(ClientPid, State#channel_state.clients)}
      };
    false -> {reply, user_not_joined, State}
  end;
% Handle a channel message request
channel(State, {message_send, ClientPid, ClientNick, Message}) ->
  case lists:member(ClientPid, State#channel_state.clients) of
    true ->
      broadcast_message(
        State#channel_state.name,
        ClientPid,
        ClientNick,
        Message,
        State#channel_state.clients
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

% Check if a client with a given nick exists in a list of clients
nick_exists(_, []) -> false;
nick_exists(Nick, [Client | RestOfClients]) ->
  case Nick == Client#client.nick of
    true -> true;
    false -> nick_exists(Nick, RestOfClients)
  end.

% Check if a client with a given pid exists in a list of clients
pid_exists(_, []) -> false;
pid_exists(Pid, [Client | RestOfClients]) ->
  case Pid == Client#client.pid of
    true -> true;
    false -> nick_exists(Pid, RestOfClients)
  end.

% Check if a channel exists in a list of channels
channel_exists(Channel, Channels) -> lists:member(Channel, Channels).

% Update a client to use a new nick
update_client_nick(_, _, []) -> [];
update_client_nick(NewNick, ClientPid, [Client | RestOfClients]) ->
  if
    ClientPid == Client#client.pid -> [Client#client{nick = NewNick} | RestOfClients];
    true -> [Client | update_client_nick(NewNick, ClientPid, RestOfClients)]
  end.

% Check that a client trying to join a channel has a unique nick
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

% Check if a channel exists, else create it
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

% Join a channel
join_channel(State, {ChannelName, ClientPid, _}) ->
  {genserver:request(list_to_atom(ChannelName), {join, ClientPid}), State}.

% Broadcast a message to all clients in a channel
broadcast_message(ChannelName, ClientPid, ClientNick, Message, Clients) ->
  spawn(
    fun
      () ->
        [
          broadcast_message_to_client(Pid, ChannelName, ClientPid, ClientNick, Message)
          || Pid <- Clients
        ]
    end
  ).

% Broadcast a message to a client in a server
broadcast_message_to_client(Pid, ChannelName, ClientPid, ClientNick, Message) ->
  if
    Pid == ClientPid -> skip;
    true -> genserver:request(Pid, {message_receive, ChannelName, ClientNick, Message})
  end.
