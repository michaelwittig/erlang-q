%%%-----------------------------------------------------------------------------
%%% @author Michael Wittig <post@michaelwittig.info>
%%% @doc Demo gen_event callback implementation.
%%% @end
%%%-----------------------------------------------------------------------------
-module(q_demo_handler).

-include("global.hrl").

-record(state, {}).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------
%% gen_event callbacks
%%------------------------------------------------------------------------------

init([]) ->
	{ok, #state{}}.

handle_event({q, Table, Data}, State) ->
	io:fwrite("Q <~p> ~p~n", [Table, Data]),
	{ok, State};
handle_event(Event, State) ->
  io:fwrite("Q <unknown> ~p~n", [Event]),
	{ok, State}.

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
