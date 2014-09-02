%%%-----------------------------------------------------------------------------
%%% @author Michael Wittig <michael.wittig@tullius-walden.com>
%%% @copyright 2014 Tullius Walden Bank AG
%%% @doc Q interfacing with Erlang
%%% @end
%%%-----------------------------------------------------------------------------
-module(q).

-include("global.hrl").

-define(ANONYMOUS_USER, <<"anonymous">>).

%% API
-export([connect/5, connect/4, connect/3,  connect/2, close/1]).
-export([execute/2, execute/3, execute/4, execute/5]).
-export([subscribe/3, subscribe/2, unsubscribe/3]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Connect with username and password can be run in subscribe mode.
%%
%% @spec connect({Handler::Module | {Module,Id}, HandlerArgs::any()}, Host::string(), Port::number(), User::string(), Passwd::string()) -> {ok, pid()} | ignore | {error, string()}
%% @end
%%------------------------------------------------------------------------------
connect({Handler, HandlerArgs}, Host, Port, User, Passwd) ->
	Auth = <<User/binary, ":", Passwd/binary>>,
	{ok, EventMgrRef} = gen_event:start_link(),
	gen_event:add_handler(EventMgrRef, Handler, HandlerArgs),
	q_server:start_link([EventMgrRef, Host, Port, Auth]).

%%------------------------------------------------------------------------------
%% @doc Connect with username and password.
%%
%% @spec connect(Host::string(), Port::number(), User::string(), Passwd::string()) -> {ok, pid()} | ignore | {error, string()}
%% @end
%%------------------------------------------------------------------------------
connect (Host, Port, User, Passwd) ->
	Auth = <<User, ":", Passwd/binary>>,
	q_server:start_link([none, Host, Port, Auth]).

%%------------------------------------------------------------------------------
%% @doc Connect can be run in subscribe mode.
%%
%% @spec connect({Handler::Module | {Module,Id}, HandlerArgs::any()}, Host::string(), Port::number()) -> {ok, pid()} | ignore | {error, string()}
%% @end
%%------------------------------------------------------------------------------
connect({Handler, HandlerArgs}, Host, Port) ->
	{ok, EventMgrRef} = gen_event:start_link(),
	gen_event:add_handler(EventMgrRef, Handler, HandlerArgs),
	q_server:start_link([EventMgrRef, Host, Port, ?ANONYMOUS_USER]).

%%------------------------------------------------------------------------------
%% @doc Connect.
%%
%% @spec connect(Host::string(), Port::number()) -> {ok, pid()} | ignore | {error, string()}
%% @end
%%------------------------------------------------------------------------------
connect(Host, Port) ->
	q_server:start_link([none, Host, Port, ?ANONYMOUS_USER]).

%%------------------------------------------------------------------------------
%% @doc Close connection.
%%
%% @spec close(Server::pid()) -> ok
%% @end
%%------------------------------------------------------------------------------
close(Server) ->
	gen_server:call(Server, close).

%%------------------------------------------------------------------------------
%% @doc Executes Q code sync remotly and returns the deserialized data structure.
%%
%% @spec execute(Server::pid(), Q::string()) -> any() | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
execute(Server, Q) ->
	gen_server:call(Server, {execute, Q}).

%%------------------------------------------------------------------------------
%% @doc Executes Q function with one argument sync remotly and returns the deserialized data structure.
%%
%% @spec execute(Server::pid(), Fun::string(), SerializedX::binary()) -> any() | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
execute(Server, Fun, SerializedX) ->
	gen_server:call(Server, {execute, Fun, SerializedX}).

%%------------------------------------------------------------------------------
%% @doc Executes Q function with two argument sync remotly and returns the deserialized data structure.
%%
%% @spec execute(Server::pid(), Fun::string(), SerializedX::binary(), SerializedY::binary()) -> any() | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
execute(Server, Fun, SerializedX, SerializedY) ->
	gen_server:call(Server, {execute, Fun, SerializedX, SerializedY}).

%%------------------------------------------------------------------------------
%% @doc Executes Q function with three argument sync remotly and returns the deserialized data structure.
%%
%% @spec execute(Server::pid(), Fun::string(), SerializedX::binary(), SerializedY::binary(), SerializedZ::binary()) -> any() | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
execute(Server, Fun, SerializedX, SerializedY, SerializedZ) ->
	gen_server:call(Server, {execute, Fun, SerializedX, SerializedY, SerializedZ}).

%%------------------------------------------------------------------------------
%% @doc Subscribe to a table-symbol combination.
%%
%% @spec subscribe(Server::pid(), Table::string(), Symbol::string()) -> ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
subscribe(Server, Table, Symbol) ->
	gen_server:call(Server, {subscribe, Table, Symbol}).

%%------------------------------------------------------------------------------
%% @doc Subscribe to multiple table-symbol combinations like [{<<"table">>, <<"sym">>}, {<<"table">>, <<"sym">>}].
%%
%% @spec subscribe(Server::pid(), Subscriptions::[any()]) -> ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
subscribe(Server, Subscriptions) ->
	gen_server:call(Server, {subscribe, Subscriptions}).

%%------------------------------------------------------------------------------
%% @doc Unsubscribe from a table-symbol combination.
%%
%% @spec unsubscribe(Server::pid(), Table::string(), Symbol::string()) -> ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
unsubscribe(Server, Table, Symbol) ->
	gen_server:call(Server, {unsubscribe, Table, Symbol}).

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
execute_q_atom_test() ->
	{ok, Pid} = connect(<<"localhost">>, 5010),
	?assertEqual(2, execute(Pid, <<"1+1">>)).

execute_q_vector_test() ->
	{ok, Pid} = connect(<<"localhost">>, 5010),
	?assertEqual([0, 1, 2, 3, 4], execute(Pid, <<"til 5">>)).

execute_fun_test() ->
	{ok, Pid} = connect(<<"localhost">>, 5010),
	?assertEqual(15, execute(Pid, <<"sum">>, q_ipcp:serialize_ints([1, 2, 3, 4, 5]))).

execute_subscribe_test() ->
	{ok, Pid} = connect({q_demo_handler, []}, <<"localhost">>, 5010),
	?assertEqual(ok, subscribe(Pid, <<"trade">>, <<"DAI">>)).

execute_subscribes_test() ->
	{ok, Pid} = connect({q_demo_handler, []}, <<"localhost">>, 5010),
	?assertEqual(ok, subscribe(Pid, [{<<"trade">>, <<"DAI">>}, {<<"trade">>, <<"BMW">>}])).

connect_and_close_tes_t() -> % TODO test fails because the server is correctly terminated
	{ok, Pid} = connect(<<"localhost">>, 5010),
	ok = close(Pid).
