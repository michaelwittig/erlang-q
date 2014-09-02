%%%-----------------------------------------------------------------------------
%%% @author Michael Wittig <michael.wittig@tullius-walden.com>
%%% @copyright 2014 Tullius Walden Bank AG
%%% @doc Q connection server.
%%% @end
%%%-----------------------------------------------------------------------------
-module(q_server).

-include("global.hrl").

-define(SOCKET_OPTS, [binary, {active, once}, {packet, raw}, {reuseaddr, true}]).
-define(RECV_TIMEOUT, 5000).
-define(CONNECT_TIMEOUT, 5000).

-record(state, {sock :: any(), mode :: atom(), subscribes = sets:new() :: any(), event_mgr_ref :: any(), rest = <<>> :: binary() }).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @doc Starts the server. EventMgrRef::pid(), Host::string(), Port::number(), Auth::string()
%%
%% @spec start_link(list()) -> {ok, pid()} | ignore | {error, string()}
%% @end
%%-----------------------------------------------------------------------------
start_link([EventMgrRef, Host, Port, Auth]) ->
  gen_server:start_link(?MODULE, [EventMgrRef, Host, Port, Auth], []).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([EventMgrRef, HostRaw, Port, AuthRaw]) ->
  Host = binary_or_string_to_string(HostRaw),
  Auth = binary_or_string_to_binary(AuthRaw),
  {ok, Sock} = gen_tcp:connect(Host, Port, ?SOCKET_OPTS, ?CONNECT_TIMEOUT),
  State = #state{sock = Sock, mode = execute, event_mgr_ref = EventMgrRef},
  ok = inet:setopts(Sock, [{active, false}]),
  case gen_tcp:send(Sock, <<Auth/binary, 3, 0>>) of
    ok ->
      case gen_tcp:recv(Sock, 1, ?RECV_TIMEOUT) of
        {ok, <<Capability:8/unsigned-integer>>} ->
          q_ipcp:capability(Capability),
          {ok, State};
        {error, Reason} -> {error, Reason}
      end;
    {error, Reason} -> {error, Reason}
  end.

handle_call(close, _From, State) ->
  close(State),
  {stop, "close was called", ok, State};
handle_call({execute, Q}, _From, State) ->
  case State#state.mode of
    execute ->
      Res = execute_bin(State, q_ipcp:serialize(sync, q_ipcp:serialize_string(Q))),
      {reply, Res, State};
    _ ->
      {reply, {error, "Not in execute mode"}, State}
  end;
handle_call({execute, Fun, SerializedX}, _From, State) ->
  case State#state.mode of
    execute ->
      Res = execute_bin(State, q_ipcp:serialize(sync, q_ipcp:serialize_generallist([q_ipcp:serialize_string(Fun), SerializedX]))),
      {reply, Res, State};
    _ ->
      {reply, {error, "Not in execute mode"}, State}
  end;
handle_call({execute, Fun, SerializedX, SerializedY}, _From, State) ->
  case State#state.mode of
    execute ->
      Res = execute_bin(State, q_ipcp:serialize(sync, q_ipcp:serialize_generallist([q_ipcp:serialize_string(Fun), SerializedX, SerializedY]))),
      {reply, Res, State};
    _ ->
      {reply, {error, "Not in execute mode"}, State}
  end;
handle_call({execute, Fun, SerializedX, SerializedY, SerializedZ}, _From, State) ->
  case State#state.mode of
    execute ->
      Res = execute_bin(State, q_ipcp:serialize(sync, q_ipcp:serialize_generallist([q_ipcp:serialize_string(Fun), SerializedX, SerializedY, SerializedZ]))),
      {reply, Res, State};
    _ ->
      {reply, {error, "Not in execute mode"}, State}
  end;
handle_call({subscribe, Table, Symbol}, _From, State) ->
  case State#state.event_mgr_ref of
    none -> {reply, {error, "No EventMgrRef provided"}, State};
    _ ->
      NewSubscribes = sets:add_element({Table, Symbol}, State#state.subscribes),
      case execute_subscribe(State, NewSubscribes) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
      end
  end;
handle_call({subscribe, Subscriptions}, _From, State) ->
  case State#state.event_mgr_ref of
    none -> {reply, {error, "No EventMgrRef provided"}, State};
    _ ->
      NewSubscribes = sets:union(State#state.subscribes, sets:from_list(Subscriptions)),
      case execute_subscribe(State, NewSubscribes) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
      end
  end;
handle_call({unsubscribe, Table, Symbol}, _From, State) ->
  case State#state.mode of
    subscribe ->
      NewSubscribes = sets:del_element({Table, Symbol}, State#state.subscribes),
      case execute_subscribe(State, NewSubscribes) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
      end;
    _ ->
      {reply, {error, "Not in subscribe mode"}, State}
  end.

handle_cast(Any, State) ->
  error_logger:info_msg("unhaldled cast ~p~n", [Any]),
  {noreply, State}.

handle_info({tcp, _, Bin}, State = #state{rest = Rest}) ->
  {Content, NewRest} = q_ipcp:deserialize(<<Rest/binary, Bin/binary>>),
  ok = case Content of
    [<<"upd">>, Table, Data] ->
      gen_event:notify(State#state.event_mgr_ref, {q, Table, Data});
    _ ->
      error_logger:info_msg("unexpected content ~p~n", [Content]),
      ok
  end,
  {noreply, State#state{rest = NewRest}};
handle_info(Any, State) ->
  error_logger:info_msg("unhaldled info ~p~n", [Any]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

binary_or_string_to_string(Str) when is_list(Str) ->
  Str;
binary_or_string_to_string(Str) when is_binary(Str) ->
  binary_to_list(Str).

binary_or_string_to_binary(Str) when is_list(Str) ->
  list_to_binary(Str);
binary_or_string_to_binary(Str) when is_binary(Str) ->
  Str.

%%------------------------------------------------------------------------------
%% @doc Close connection.
%%
%% @spec close(State::#state{}) -> ok
%% @end
%%------------------------------------------------------------------------------
close(State) ->
  Sock = State#state.sock,
  ok = gen_tcp:close(Sock).

%%------------------------------------------------------------------------------
%% @doc Executes binary sync remotly and returns the deserialized data structure.
%%
%% @spec execute_bin(State::#state{}, SendBin::binary()) -> any() | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
execute_bin(State, SendBin) ->
  RecvBin = request(State, SendBin),
  case RecvBin of
    {error, Reason} -> {error, Reason};
    _ ->
      {Content, <<>>} = q_ipcp:deserialize(RecvBin),
      Content
  end.

%%------------------------------------------------------------------------------
%% @doc make sync ipc request and return the binary answer
%%
%% @spec request(State::#state{}, Bin::binary()) -> binary() | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
request(State, Bin) when is_binary(Bin) ->
  Sock = State#state.sock,
  ok = inet:setopts(Sock, [{active, false}]),
  case gen_tcp:send(Sock, Bin) of
    ok ->
      %% Hope there's nothing else coming down on the socket..
      case gen_tcp:recv(Sock, 9, ?RECV_TIMEOUT) of
        {ok, Header} ->
          {{_Endianness, MsgLength, _Type}, <<>>} = q_ipcp:deserialize_header(Header),
          case gen_tcp:recv(Sock, MsgLength - 9, ?RECV_TIMEOUT) of
            {ok, Body} ->
              <<Header/binary, Body/binary>>;
            {error, Reason} ->
              {error, Reason}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%%------------------------------------------------------------------------------
%% @doc Resubscribe to all subsriptions.
%%
%% @spec execute_subscribe(OldState::#state{}, NewSubscribes) -> {ok, #state{}} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
execute_subscribe(OldState, NewSubscribes) ->
  List = sets:to_list(NewSubscribes),
  Tables = sets:to_list(sets:from_list(lists:map(fun({Table, _Symbol}) -> <<"`", Table/binary>> end, List))),
  Symbols = sets:to_list(sets:from_list(lists:map(fun({_Table, Symbol}) -> <<"`", Symbol/binary>> end, List))),
  execute_subscribe(OldState, NewSubscribes, Tables, Symbols).

execute_subscribe(OldState, NewSubscribes, [], _Symbols) ->
  NewState = OldState#state{mode = subscribe, subscribes = NewSubscribes},
  {ok, NewState};
execute_subscribe(OldState, NewSubscribes, [Table | Tables], []) ->
  Q = <<".u.sub[", Table/binary, ";`]">>,
  ok = case OldState#state.mode of
    subscribe -> ok;
    _ -> inet:setopts(OldState#state.sock, [{active, true}]) % from now on tcp requests are handled in the handle_info({tcp, ...}) callbacks
  end,
  ok = gen_tcp:send(OldState#state.sock, q_ipcp:serialize(sync, q_ipcp:serialize_string(Q))),
  NewState = OldState#state{mode = subscribe},
  execute_subscribe(NewState, NewSubscribes, Tables, []);
execute_subscribe(OldState, NewSubscribes, [Table | Tables], Symbols) ->
  SymbolsBin = lists:foldr(fun(A, B) -> <<A/binary, B/binary>> end, <<>>, Symbols),
  Q = <<".u.sub[", Table/binary, ";", SymbolsBin/binary, "]">>,
  ok = case OldState#state.mode of
    subscribe -> ok;
    _ -> inet:setopts(OldState#state.sock, [{active, true}]) % from now on tcp requests are handled in the handle_info({tcp, ...}) callbacks
  end,
  ok = gen_tcp:send(OldState#state.sock, q_ipcp:serialize(sync, q_ipcp:serialize_string(Q))),
  NewState = OldState#state{mode = subscribe},
  execute_subscribe(NewState, NewSubscribes, Tables, Symbols).
