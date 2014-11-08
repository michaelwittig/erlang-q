%%%-----------------------------------------------------------------------------
%%% @author Michael Wittig <michael.wittig@cinovo.de>
%%% @doc Q table helpers.
%%% @end
%%%-----------------------------------------------------------------------------
-module(q_table).

-include("global.hrl").

%% API
-export([cell/3, col/2, row/2]).
-export([cols/1, rows/1]).
-export([width/1, height/1]).
-export([map/2, map/3]).
-export([foreach/2]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Get the value of a cell. RowId is [1..cols(TableData)]
%%
%% @spec cell(TableData::any(), ColName::atom(), RowId::number()) -> any() | undefined
%% @end
%%------------------------------------------------------------------------------
cell(TableData, ColName, RowId) ->
  case col(TableData, ColName) of
    undefined -> undefined;
    Row ->
      case valid_row_id(RowId, Row) of
        true -> lists:nth(RowId, Row);
        false -> undefined
      end
  end.

valid_row_id(RowId, Row) when RowId>0 ->
  RowId=<length(Row);
valid_row_id(_RowId, _Row) ->
  false.

%%------------------------------------------------------------------------------
%% @doc Get the values of a col.
%%
%% @spec col(TableData::any(), ColName::atom()) -> [any()] | undefined
%% @end
%%------------------------------------------------------------------------------
col(TableData, ColName) ->
  Dict = dict:from_list(TableData),
  case dict:find(ColName, Dict) of
    error -> undefined;
    {ok, Row} ->
      Row
  end.

%%------------------------------------------------------------------------------
%% @doc Get the values of a row. RowId is [1..cols(TableData)]
%%
%% @spec row(TableData::any(), RowId::number()) -> [any()] | undefined
%% @end
%%------------------------------------------------------------------------------
row(TableData, RowId) ->
  Cols = cols(TableData),
  lists:map(fun(ColName) -> q_table:cell(TableData, ColName, RowId) end, Cols).

%%------------------------------------------------------------------------------
%% @doc Cols of the table.
%%
%% @spec cols(TableData::any()) -> [atom()]
%% @end
%%------------------------------------------------------------------------------
cols(TableData) ->
  lists:map(fun({ColName, _Row}) -> ColName end, TableData).

%%------------------------------------------------------------------------------
%% @doc Rows of the table in list form.
%%
%% @spec rows(TableData::any()) -> [any()]
%% @end
%%------------------------------------------------------------------------------
rows(TableData) ->
  lists:reverse(rows(TableData, height(TableData))).

%%------------------------------------------------------------------------------
%% @doc Number of cols of the table.
%%
%% @spec width(TableData::any()) -> number()
%% @end
%%------------------------------------------------------------------------------
width(TableData) -> length(TableData).

%%------------------------------------------------------------------------------
%% @doc Number of rows of the table.
%%
%% @spec height(TableData::any()) -> number()
%% @end
%%------------------------------------------------------------------------------
height([]) -> 0;
height([{_ColName,Row}|_]) -> length(Row).

%%------------------------------------------------------------------------------
%% @doc Map the values of a col. RowId is [1..cols(TableData)]
%%
%% @spec map(TableData::any(), ColName::atom(), Fun::fun(any())) -> [any()] | undefined
%% @end
%%------------------------------------------------------------------------------
map(TableData, ColName, Fun) ->
  Col = col(TableData, ColName),
  case Col of
    undefined -> undefined;
    _ -> lists:map(Fun, Col)
  end.

%%------------------------------------------------------------------------------
%% @doc Map the rows of a table.
%%
%% @spec map(TableData::any(), Fun::fun([any()])) -> [any()] | undefined
%% @end
%%------------------------------------------------------------------------------
map(TableData, Fun) ->
  lists:reverse(maprows(TableData, height(TableData), Fun)).

%%------------------------------------------------------------------------------
%% @doc Calls Fun(Row) for each element of the table.
%%
%% @spec foreach(TableData::any(), Fun::fun([any()])) -> ok
%% @end
%%------------------------------------------------------------------------------
foreach(TableData, Fun) ->
  foreach(TableData, height(TableData), 1, Fun).

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

foreach(TableData, Height, Height, Fun) ->
  Row = row(TableData, Height),
  Fun(Row),
  ok;
foreach(TableData, Height, CurrentRowId, Fun) ->
  Row = row(TableData, CurrentRowId),
  Fun(Row),
  foreach(TableData, Height, CurrentRowId+1, Fun).

maprows(_TableData, 0, _Fun) ->
  [];
maprows(TableData, CurrentRowId, Fun) ->
  [Fun(row(TableData, CurrentRowId))|maprows(TableData, CurrentRowId-1, Fun)].

rows(_TableData, 0) ->
  [];
rows(TableData, CurrentRowId) ->
  [row(TableData, CurrentRowId)|rows(TableData, CurrentRowId-1)].

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

cell_test() ->
  TableData = [{<<"sym">>,[<<"a">>, <<"ab">>, <<"abc">>]},{<<"price">>,[10, 20, 30]}],
  ?assertEqual(<<"a">>, cell(TableData, <<"sym">>, 1)),
  ?assertEqual(10, cell(TableData, <<"price">>, 1)),
  ?assertEqual(undefined, cell(TableData, <<"sym">>, 0)), % invalid row id
  ?assertEqual(undefined, cell(TableData, <<"sym">>, 4)), % invalid row id
  ?assertEqual(undefined, cell(TableData, <<"xxx">>, 1)). % invalid col name

col_test() ->
  TableData = [{<<"sym">>,[<<"a">>, <<"ab">>, <<"abc">>]},{<<"price">>,[10, 20, 30]}],
  ?assertEqual([<<"a">>, <<"ab">>, <<"abc">>], col(TableData, <<"sym">>)).

row_test() ->
  TableData = [{<<"sym">>,[<<"a">>, <<"ab">>, <<"abc">>]},{<<"price">>,[10, 20, 30]}],
  ?assertEqual([<<"a">>, 10], row(TableData, 1)).

rows_test() ->
  TableData = [{<<"sym">>,[<<"a">>, <<"ab">>, <<"abc">>]},{<<"price">>,[10, 20, 30]}],
  ?assertEqual([[<<"a">>, 10], [<<"ab">>, 20], [<<"abc">>, 30]], rows(TableData)).

cols_test() ->
  TableData = [{<<"sym">>,[<<"a">>, <<"ab">>, <<"abc">>]},{<<"price">>,[10, 20, 30]}],
  ?assertEqual([<<"sym">>, <<"price">>], cols(TableData)).

width_test() ->
TableData = [{<<"sym">>,[<<"a">>, <<"ab">>, <<"abc">>]},{<<"price">>,[10, 20, 30]}],
  ?assertEqual(2, width(TableData)).

height_test() ->
  TableData = [{<<"sym">>,[<<"a">>, <<"ab">>, <<"abc">>]},{<<"price">>,[10, 20, 30]}],
  ?assertEqual(3, height(TableData)).

map_col_test() ->
  TableData = [{<<"sym">>,[<<"a">>, <<"ab">>, <<"abc">>]},{<<"price">>,[10, 20, 30]}],
  ?assertEqual([20, 40, 60], map(TableData, <<"price">>, fun(Price) -> 2*Price end)).

map_row_test() ->
  TableData = [{<<"sym">>,[<<"a">>, <<"ab">>, <<"abc">>]},{<<"price">>,[10, 20, 30]}],
  ?assertEqual([[<<"a">>, 20], [<<"ab">>, 40], [<<"abc">>, 60]], map(TableData, fun([Sym, Price]) -> [Sym, 2*Price] end)).

foreach_test() ->
  TableData = [{<<"sym">>,[<<"a">>, <<"ab">>, <<"abc">>]},{<<"price">>,[10, 20, 30]}],
  ?assertEqual(ok, foreach(TableData, fun([Sym, Price]) -> erlang:display([Sym, Price]) end)).
