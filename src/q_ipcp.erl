%%%-----------------------------------------------------------------------------
%%% @author Michael Wittig <post@michaelwittig.info>
%%% @todo decompression of received bytes if they are compressed
%%% @doc Implements the Q ipc protocol http://code.kx.com/wiki/Reference/q_ipcprotocol
%%% @end
%%%-----------------------------------------------------------------------------
-module(q_ipcp).

-include("global.hrl").

%% API
-export([deserialize/1, deserialize_header/1]).
-export([serialize/2]).
-export([serialize_boolean/1, serialize_booleans/1]).
-export([serialize_guid/1, serialize_guids/1]).
-export([serialize_byte/1, serialize_bytes/1]).
-export([serialize_short/1, serialize_shorts/1]).
-export([serialize_int/1, serialize_ints/1]).
-export([serialize_long/1, serialize_longs/1]).
-export([serialize_real/1, serialize_reals/1]).
-export([serialize_float/1, serialize_floats/1]).
-export([serialize_char/1, serialize_chars/1, serialize_string/1]).
-export([serialize_symbol/1, serialize_symbols/1]).
-export([serialize_timestamp/1, serialize_timestamps/1]).
-export([serialize_month/1, serialize_months/1]).
-export([serialize_date/1, serialize_dates/1]).
-export([serialize_datetime/1, serialize_datetimes/1]).
-export([serialize_timespan/1, serialize_timespans/1]).
-export([serialize_minute/1, serialize_minutes/1]).
-export([serialize_second/1, serialize_seconds/1]).
-export([serialize_time/1, serialize_times/1]).
-export([serialize_generallist/1]).
-export([capability/1, endianness/1]).
-export([bin_to_hexstr/1,hexstr_to_bin/1]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc deserialize binary data
%%
%% @spec deserialize(Bin::binary()) -> {any(), binary()}
%% @end
%%------------------------------------------------------------------------------
deserialize(Bin) ->
  {{Endianness, _MsgLength, Type}, Rest} = deserialize_header(Bin),
  deserialize_body(Endianness, Type, Rest).

%%------------------------------------------------------------------------------
%% @doc serialize data
%%
%% @spec serialize(MsgType::atom() | number(), Body::binary()) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize(async, Body) -> serialize(0, Body);
serialize(sync, Body) -> serialize(1, Body);
serialize(response, Body) -> serialize(2, Body);
serialize(MsgType, Body) ->
  MsgLength = 8 + byte_size(Body),
  Header = <<1, MsgType:8/unsigned-integer, 0, 0, MsgLength:32/little-unsigned-integer>>,
  <<Header/binary, Body/binary>>.

%%------------------------------------------------------------------------------
%% @doc create boolean type
%%
%% @spec serialize_boolean(Value::true | false) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_boolean(true) -> serialize_body(-1, <<1:8/unsigned-integer>>);
serialize_boolean(false) -> serialize_body(-1, <<0:8/unsigned-integer>>).

%%------------------------------------------------------------------------------
%% @doc create boolean vector type
%%
%% @spec serialize_booleans(Values::[true | false]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_booleans(Values) when is_list(Values) ->
  serialize_vector(1, Values, fun serialize_boolean/1).

%%------------------------------------------------------------------------------
%% @doc create guid type
%%
%% @spec serialize_guid(Bin::ninary() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_guid(null) ->
  serialize_body(-2, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>);
serialize_guid(<<Bin:128/bits>>) ->
  serialize_body(-2, Bin).

%%------------------------------------------------------------------------------
%% @doc create guid vector type
%%
%% @spec serialize_guids(Bins::[binary()]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_guids(Bins) when is_list(Bins) ->
  serialize_vector(2, Bins, fun serialize_guid/1).

%%------------------------------------------------------------------------------
%% @doc create byte type
%%
%% @spec serialize_byte(Byte::binary()) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_byte(<<Byte:8/bits>>) -> serialize_body(-4, Byte).

%%------------------------------------------------------------------------------
%% @doc create byte vector type
%%
%% @spec serialize_bytes(Values::[binary()]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_bytes(Values) when is_list(Values) ->
  serialize_vector(4, Values, fun serialize_byte/1).

%%------------------------------------------------------------------------------
%% @doc create short type
%%
%% @spec serialize_short(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_short(null) -> serialize_body(-5, <<-32768:16/little-signed-integer>>);
serialize_short(Number) -> serialize_body(-5, <<Number:16/little-signed-integer>>).

%%------------------------------------------------------------------------------
%% @doc create short vector type
%%
%% @spec serialize_shorts(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_shorts(Numbers) when is_list(Numbers) ->
  serialize_vector(5, Numbers, fun serialize_short/1).

%%------------------------------------------------------------------------------
%% @doc create int type
%%
%% @spec serialize_int(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_int(null) -> serialize_body(-6, <<-2147483648:32/little-signed-integer>>);
serialize_int(Number) -> serialize_body(-6, <<Number:32/little-signed-integer>>).

%%------------------------------------------------------------------------------
%% @doc create int vector type
%%
%% @spec serialize_ints(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_ints(Numbers) when is_list(Numbers) ->
  serialize_vector(6, Numbers, fun serialize_int/1).

%%------------------------------------------------------------------------------
%% @doc create long type
%%
%% @spec serialize_long(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_long(null) -> serialize_body(-7, <<-9223372036854775808:64/little-signed-integer>>);
serialize_long(Number) -> serialize_body(-7, <<Number:64/little-signed-integer>>).

%%------------------------------------------------------------------------------
%% @doc create long vector type
%%
%% @spec serialize_longs(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_longs(Numbers) when is_list(Numbers) ->
  serialize_vector(7, Numbers, fun serialize_long/1).

%%------------------------------------------------------------------------------
%% @doc create real type
%%
%% @spec serialize_real(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_real(null) -> serialize_body(-8, <<0, 0, 192, 255>>);
serialize_real(Number) -> serialize_body(-8, <<Number:32/little-signed-float>>).

%%------------------------------------------------------------------------------
%% @doc create real vector type
%%
%% @spec serialize_reals(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_reals(Numbers) when is_list(Numbers) ->
  serialize_vector(8, Numbers, fun serialize_real/1).

%%------------------------------------------------------------------------------
%% @doc create float type
%%
%% @spec serialize_float(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_float(null) -> serialize_body(-9, <<0,0,0,0,0,0,248,255>>);
serialize_float(Number) -> serialize_body(-9, <<Number:64/little-signed-float>>).

%%------------------------------------------------------------------------------
%% @doc create float vector type
%%
%% @spec serialize_floats(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_floats(Numbers) when is_list(Numbers) ->
  serialize_vector(9, Numbers, fun serialize_float/1).

%%------------------------------------------------------------------------------
%% @doc create char type
%%
%% @spec serialize_char(Char::char() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_char(null) ->
  serialize_char(<<" ">>);
serialize_char(Char) when is_binary(Char) ->
  serialize_body(-10, Char);
serialize_char(Char) when is_list(Char) ->
  serialize_body(-10, list_to_binary(Char)).

%%------------------------------------------------------------------------------
%% @doc create char vector type
%%
%% @spec serialize_string(String::string()) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_string(String) when is_binary(String) ->
  VectorLength = size(String),
  serialize_body(10, <<0, VectorLength:32/little-unsigned-integer, String/binary>>);
serialize_string(String) when is_list(String) ->
  serialize_string(list_to_binary(String)).

%%------------------------------------------------------------------------------
%% @doc create char vector type
%%
%% @spec serialize_chars(Chars::[char()] | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_chars(Chars) when is_list(Chars) ->
  StringBin = list_to_binary(lists:flatten(Chars)),
  VectorLength = length(Chars),
  serialize_body(10, <<0, VectorLength:32/little-unsigned-integer, StringBin/binary>>).

serialize_symbol(null) ->
  serialize_symbol(<<"">>);
serialize_symbol(Atom) when is_atom(Atom) ->
  serialize_symbol(atom_to_list(Atom));
serialize_symbol(String) when is_binary(String) ->
  serialize_body(-11, <<String/binary, 0>>);
serialize_symbol(String) when is_list(String) ->
  StringBin = list_to_binary(String),
  serialize_symbol(StringBin).

%%------------------------------------------------------------------------------
%% @doc create symbol vector type
%%
%% @spec serialize_symbols(Strings::[string() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_symbols(Strings) when is_list(Strings) ->
  serialize_vector(11, Strings, fun serialize_symbol/1).

%%------------------------------------------------------------------------------
%% @doc create timestamp type
%%
%% @spec serialize_timestamp(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_timestamp(null) -> serialize_body(-12, <<-9223372036854775808:64/little-signed-integer>>);
serialize_timestamp(Number) -> serialize_body(-12, <<Number:64/little-signed-integer>>).

%%------------------------------------------------------------------------------
%% @doc create timestamp vector type
%%
%% @spec serialize_timestamps(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_timestamps(Numbers) when is_list(Numbers) ->
  serialize_vector(12, Numbers, fun serialize_timestamp/1).

%%------------------------------------------------------------------------------
%% @doc create month type
%%
%% @spec serialize_month(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_month(null) -> serialize_body(-13, <<-2147483648:32/little-signed-integer>>);
serialize_month(Number) -> serialize_body(-13, <<Number:32/little-signed-integer>>).

%%------------------------------------------------------------------------------
%% @doc create month vector type
%%
%% @spec serialize_months(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_months(Numbers) when is_list(Numbers) ->
  serialize_vector(13, Numbers, fun serialize_month/1).

%%------------------------------------------------------------------------------
%% @doc create date type
%%
%% @spec serialize_date(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_date(null) -> serialize_body(-14, <<-2147483648:32/little-signed-integer>>);
serialize_date(Number) -> serialize_body(-14, <<Number:32/little-signed-integer>>).

%%------------------------------------------------------------------------------
%% @doc create date vector type
%%
%% @spec serialize_dates(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_dates(Numbers) when is_list(Numbers) ->
  serialize_vector(14, Numbers, fun serialize_date/1).

%%------------------------------------------------------------------------------
%% @doc create datetime type
%%
%% @spec serialize_datetime(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_datetime(null) -> serialize_body(-15, <<-2251799813685248:64/little-signed-integer>>);
serialize_datetime(Number) -> serialize_body(-15, <<Number:64/little-signed-integer>>).

%%------------------------------------------------------------------------------
%% @doc create datetime vector type
%%
%% @spec serialize_datetimes(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_datetimes(Numbers) when is_list(Numbers) ->
  serialize_vector(15, Numbers, fun serialize_datetime/1).

%%------------------------------------------------------------------------------
%% @doc create timespan type
%%
%% @spec serialize_timespan(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_timespan(null) -> serialize_body(-16, <<-9223372036854775808:64/little-signed-integer>>);
serialize_timespan(Number) -> serialize_body(-16, <<Number:64/little-signed-integer>>).

%%------------------------------------------------------------------------------
%% @doc create timespan vector type
%%
%% @spec serialize_timespans(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_timespans(Numbers) when is_list(Numbers) ->
  serialize_vector(16, Numbers, fun serialize_timespan/1).

%%------------------------------------------------------------------------------
%% @doc create minute type
%%
%% @spec serialize_minute(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_minute(null) -> serialize_body(-17, <<-2147483648:32/little-signed-integer>>);
serialize_minute(Number) -> serialize_body(-17, <<Number:32/little-signed-integer>>).

%%------------------------------------------------------------------------------
%% @doc create minute vector type
%%
%% @spec serialize_minutes(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_minutes(Numbers) when is_list(Numbers) ->
  serialize_vector(17, Numbers, fun serialize_minute/1).

%%------------------------------------------------------------------------------
%% @doc create second type
%%
%% @spec serialize_second(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_second(null) -> serialize_body(-18, <<-2147483648:32/little-signed-integer>>);
serialize_second(Number) -> serialize_body(-18, <<Number:32/little-signed-integer>>).

%%------------------------------------------------------------------------------
%% @doc create second vector type
%%
%% @spec serialize_seconds(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_seconds(Numbers) when is_list(Numbers) ->
  serialize_vector(18, Numbers, fun serialize_second/1).

%%------------------------------------------------------------------------------
%% @doc create time type
%%
%% @spec serialize_time(Number::number() | null) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_time(null) -> serialize_body(-19, <<-2147483648:32/little-signed-integer>>);
serialize_time(Number) -> serialize_body(-19, <<Number:32/little-signed-integer>>).

%%------------------------------------------------------------------------------
%% @doc create time vector type
%%
%% @spec serialize_times(Numbers::[number() | null]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_times(Numbers) when is_list(Numbers) ->
  serialize_vector(19, Numbers, fun serialize_time/1).

%%------------------------------------------------------------------------------
%% @doc create generallist type
%%
%% @spec serialize_generallist(SerializedValues::[binary()]) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_generallist(SerializedValues) ->
  ListLength = length(SerializedValues),
  Bin = list_to_binary(SerializedValues),
  serialize_body(0, <<0, ListLength:32/little-unsigned-integer, Bin/binary>>).

%%------------------------------------------------------------------------------
%% @doc deserialize binary header data
%%
%% @spec deserialize_header(Bin::binary()) -> {{atom(), number(), number()}, binary()}
%% @end
%%------------------------------------------------------------------------------
deserialize_header(<<Arch:8/unsigned-integer, _MsgType:8/bits, _Reserved:16/bits, Bin/bits>>) ->
  Endianness = endianness(Arch),
  {MsgLength, Type} = case Endianness of
    big ->
      <<MsgLength1:32/big-unsigned-integer, Type1:8/signed-integer, Rest/bits>> = Bin,
      {MsgLength1, Type1};
    little ->
      <<MsgLength1:32/little-unsigned-integer, Type1:8/signed-integer, Rest/bits>> = Bin,
      {MsgLength1, Type1}
  end,
  {{Endianness, MsgLength, Type}, Rest}.

%%------------------------------------------------------------------------------
%% @doc ipc protocol capability to atom
%%
%% @spec capability(CapabilityCode::number()) -> [atom()]
%% @end
%%------------------------------------------------------------------------------
capability(0) ->
  [];
capability(1) ->
  [compression, timestamp, timespan];
capability(2) ->
  [compression, timestamp, timespan];
capability(3) ->
  [compression, timestamp, timespan, uuid].

%%------------------------------------------------------------------------------
%% @doc ipc protocol endianness to atom
%%
%% @spec endianness(EndiannessCode::number()) -> big | little
%% @end
%%------------------------------------------------------------------------------
endianness(0) ->
  big;
endianness(1) ->
  little.

bin_to_hexstr(Bin) ->
    list_to_hexstr(binary_to_list(Bin)).

hexstr_to_bin(S) ->
    list_to_binary(hexstr_to_list(S)).

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc serialize vector data
%%
%% @spec serialize_vector(VectorType::number(), Values::list(), F::fun()) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_vector(VectorType, Values, F) ->
  Fun = fun(Value) -> <<_:8/bits, ValueBin/binary>> = F(Value), ValueBin end,
  ValuesBins = lists:map(Fun, Values),
  ValuesBin = list_to_binary(ValuesBins),
  VectorLength = length(Values),
  serialize_body(VectorType, <<0, VectorLength:32/little-unsigned-integer, ValuesBin/binary>>).

%%------------------------------------------------------------------------------
%% @doc serialize binary body data
%%
%% @spec serialize_body(Type::async | sync | response, Bin::binary()) -> binary()
%% @end
%%------------------------------------------------------------------------------
serialize_body(Type, Bin) ->
  <<Type:8/signed-integer, Bin/binary>>.

%%------------------------------------------------------------------------------
%% @doc deserialize binary body data
%%
%% @spec deserialize_body(Endianness::atom(), Type::number(), Bin::binary()) -> {any(), binatry()}
%% @end
%%------------------------------------------------------------------------------
deserialize_body(Endianness, Type, Bin) ->
  parse_body(Endianness, Type, Bin).

%%------------------------------------------------------------------------------
%% @doc Combines a list of keys and a list of values into a list of {key, value}
%%
%% @spec keys_and_values_to_list(Keys::[any()], Values::[any()]) -> [any()]
%% @end
%%------------------------------------------------------------------------------
keys_and_values_to_list([], []) ->
  [];
keys_and_values_to_list([Key|Keys], [Value|Values]) ->
  [{Key, Value} | keys_and_values_to_list(Keys, Values)].

%%------------------------------------------------------------------------------
%% @doc Parses the content of a list one item at a time
%%
%% @spec parse_vector_body(Endianness::atom(), Type::number(), VectorLength::number(), Bin::binary(), Content::[any()]) -> {[any()], binary()}
%% @end
%%------------------------------------------------------------------------------
parse_vector_body(_Endianness, _Type, 0, Rest, Content) ->
  {Content, Rest};
parse_vector_body(Endianness, Type, VectorLength, Bin, Content) ->
  {ValueContent, ValueRest} = parse_body(Endianness, 0-Type, Bin),
  parse_vector_body(Endianness, Type, VectorLength-1, ValueRest, [ValueContent | Content]).

%%------------------------------------------------------------------------------
%% @doc Parses the content of a vector
%%
%% @spec parse_vector_body(Endianness::atom(), Type::number(), binary()) -> {[any()], binary()}
%% @end
%%------------------------------------------------------------------------------
parse_vector_body(little, Type, <<_AttributesBin:8, VectorLength:32/little-unsigned-integer, VectorBin/bits>>) ->
  {Content, Rest} = parse_vector_body(little, Type, VectorLength, VectorBin, []),
  {lists:reverse(Content), Rest};
parse_vector_body(big, Type, <<_AttributesBin:8, VectorLength:32/big-unsigned-integer, VectorBin/bits>>) ->
  {Content, Rest} = parse_vector_body(big, Type, VectorLength, VectorBin, []),
  {lists:reverse(Content), Rest}.

%%------------------------------------------------------------------------------
%% @doc Parses the content of a symbol vector. Symbols are specials because
%% they have a variable length and are zero terminated
%%
%% @spec parse_symbol_vector_body(Bin::binary(), VectorLength::number(), Symbol::string(), Symbols::list()) -> {[atom()], binary()}
%% @end
%%------------------------------------------------------------------------------
parse_symbol_vector_body(Rest, 0, <<>>, Symbols) ->
  {lists:reverse(Symbols), Rest};
parse_symbol_vector_body(<<Char:8/bits, Rest/bits>>, VectorLength, Symbol, Symbols) ->
  case Char of
    <<0>> ->
      parse_symbol_vector_body(Rest, VectorLength-1, <<>>, [Symbol|Symbols]);
    _ ->
      parse_symbol_vector_body(Rest, VectorLength, <<Symbol/binary, Char/binary>>, Symbols)
  end.

%%------------------------------------------------------------------------------
%% @doc Parses the content of a dictionary.
%%
%% @spec parse_dict_body(Endianness::atom(), Bin::binary()) -> {[any()], binary()}
%% @end
%%------------------------------------------------------------------------------
parse_dict_body(little, <<KeysType:8/unsigned-integer, Bin/bits>>) ->
  {Keys, <<ValuesType:8/unsigned-integer, ValuesBin/bits>>} = parse_body(little, KeysType, Bin),
  {Values, Rest} = parse_body(little, ValuesType, ValuesBin),
  {keys_and_values_to_list(Keys, Values), Rest};
parse_dict_body(big, <<KeysType:8/unsigned-integer, Bin/bits>>) ->
  {Keys, <<ValuesType:8/unsigned-integer, ValuesBin/bits>>} = parse_body(big, KeysType, Bin),
  {Values, Rest} = parse_body(big, ValuesType, ValuesBin),
  {keys_and_values_to_list(Keys, Values), Rest}.

%%------------------------------------------------------------------------------
%% @doc Parses the content of a general list.
%%
%% @spec parse_generallist_body(Endianness::atom(), Bin::binary(), ListLength::number, List::[any()]) -> {[any()], binary()}
%% @end
%%------------------------------------------------------------------------------
parse_generallist_body(_Endianness, Bin, 0, List) ->
  {lists:reverse(List), Bin};
parse_generallist_body(Endianness, <<ItemType:8/signed-integer, ListBin/bits>>, ListLength, List) ->
  {Content, Rest} = parse_body(Endianness, ItemType, ListBin),
  parse_generallist_body(Endianness, Rest, ListLength-1, [Content | List]).

%%------------------------------------------------------------------------------
%% @doc parse IPC body
%%
%% @spec parse_body(Endianness::atom(), TypeCode::number(), Bin::binary()) -> {any(), binary()}
%% @end
%%------------------------------------------------------------------------------
parse_body(little, 0, <<_AttributesBin:8/bits, ListLength:32/little-unsigned-integer, ListBin/bits>>) -> % general list
  parse_generallist_body(little, ListBin, ListLength, []);
parse_body(big, 0, <<_AttributesBin:8/bits, ListLength:32/big-unsigned-integer, ListBin/bits>>) ->
  parse_generallist_body(big, ListBin, ListLength, []);
parse_body(little, 11, <<_AttributesBin:8/bits, VectorLength:32/little-unsigned-integer, VectorBin/bits>>) -> % symbol vector
  parse_symbol_vector_body(VectorBin, VectorLength, <<>>, []);
parse_body(big, 11, <<_AttributesBin:8/bits, VectorLength:32/big-unsigned-integer, VectorBin/bits>>) ->
  parse_symbol_vector_body(VectorBin, VectorLength, [], []);
parse_body(Endianness, 98, <<_AttributesBin:8/bits, _DictType:8/bits, Bin/bits>>) -> % table
  parse_body(Endianness, 99, Bin);
parse_body(Endianness, 99, Bin) -> % dictionary
  parse_dict_body(Endianness, Bin);
parse_body(Endianness, 127, Bin) -> % sorted dictionary
  parse_dict_body(Endianness, Bin);
parse_body(Endianness, Type, Bin) when Type > 0 -> % vector
  parse_vector_body(Endianness, Type, Bin);
parse_body(_, -1, <<Content:8/unsigned-integer, Rest/bits>>) -> % boolean
  case Content of
    1 -> {true, Rest};
    0 -> {false, Rest}
  end;
parse_body(_, -2, <<Content:128/bits, Rest/binary>>) -> % guid
  case Content of
    <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>> -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(_, -4, <<Content:8/bits, Rest/bits>>) -> % byte
  {Content, Rest};
parse_body(little, -5, <<Content:16/little-signed-integer, Rest/bits>>) -> % short
  case Content of
    -32768 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(big, -5, <<Content:16/big-signed-integer, Rest/bits>>) ->
  case Content of
    -32768 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(little, -6, <<Content:32/little-signed-integer, Rest/bits>>) -> % integer
  case Content of
    -2147483648 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(big, -6, <<Content:32/big-signed-integer, Rest/bits>>) ->
  {Content, Rest};
parse_body(little, -7, <<Content:64/little-signed-integer, Rest/bits>>) -> % long
  case Content of
    -9223372036854775808 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(big, -7, <<Content:64/big-signed-integer, Rest/bits>>) ->
  case Content of
    -9223372036854775808-> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(little, -8, <<0, 0, 192, 255, Rest/bits>>) -> % real TODO big null
  {null, Rest};
parse_body(little, -8, <<Content:32/little-signed-float, Rest/bits>>) -> % real
  {Content, Rest};
parse_body(big, -8, <<Content:32/big-signed-float, Rest/bits>>) ->
  {Content, Rest};
parse_body(little, -9, <<0,0,0,0,0,0,248,255, Rest/bits>>) -> % float TODO big null
  {null, Rest};
parse_body(little, -9, <<Content:64/little-signed-float, Rest/bits>>) ->
  {Content, Rest};
parse_body(big, -9, <<Content:64/big-signed-float, Rest/bits>>) ->
  {Content, Rest};
parse_body(_, -10, <<Bin:8/bits, Rest/bits>>) -> % char
  {Bin, Rest};
parse_body(_, -11, Bin) -> % symbol
  {String, Rest} = extract_zero_terminated_string(Bin, <<>>),
  StrLength = size(String),
  case StrLength of
    0 -> {null, Rest};
    _ -> {String, Rest}
  end;
parse_body(little, -12, <<Content:64/little-signed-integer, Rest/bits>>) -> % timestamp
  case Content of
    -9223372036854775808 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(big, -12, <<Content:64/big-signed-integer, Rest/bits>>) ->
  case Content of
    -9223372036854775808 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(little, -13, <<Content:32/little-signed-integer, Rest/bits>>) -> % month
  case Content of
    -2147483648 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(big, -13, <<Content:32/big-signed-integer, Rest/bits>>) ->
  case Content of
    -2147483648 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(little, -14, <<Content:32/little-signed-integer, Rest/bits>>) -> % date
  case Content of
    -2147483648 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(big, -14, <<Content:32/big-signed-integer, Rest/bits>>) ->
  case Content of
    -2147483648 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(little, -15, <<Content:64/little-signed-integer, Rest/bits>>) -> % datetime
  case Content of
    -2251799813685248 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(big, -15, <<Content:64/big-signed-integer, Rest/bits>>) ->
  case Content of
    -2251799813685248 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(little, -16, <<Content:64/little-signed-integer, Rest/bits>>) -> % timespan
  case Content of
    -9223372036854775808 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(big, -16, <<Content:64/big-signed-integer, Rest/bits>>) ->
  case Content of
    -9223372036854775808 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(little, -17, <<Content:32/little-signed-integer, Rest/bits>>) -> % minute
  case Content of
    -2147483648 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(big, -17, <<Content:32/big-signed-integer, Rest/bits>>) ->
  case Content of
    -2147483648 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(little, -18, <<Content:32/little-signed-integer, Rest/bits>>) -> % second
  case Content of
    -2147483648 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(big, -18, <<Content:32/big-signed-integer, Rest/bits>>) ->
  case Content of
    -2147483648 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(little, -19, <<Content:32/little-signed-integer, Rest/bits>>) -> % time
  case Content of
    -2147483648 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(big, -19, <<Content:32/big-signed-integer, Rest/bits>>) ->
  case Content of
    -2147483648 -> {null, Rest};
    _ -> {Content, Rest}
  end;
parse_body(_, -128, Bin) -> % error
  {String, Rest} = extract_zero_terminated_string(Bin, <<>>),
  {{error, String}, Rest}.

extract_zero_terminated_string(<<0, Rest/binary>>, String) ->
  {String, Rest};
extract_zero_terminated_string(<<Char:8/bits, Rest/binary>>, String) ->
  extract_zero_terminated_string(Rest, <<String/binary, Char/binary>>).

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a+(N-10).

hint(C) when $0 =< C, C =< $9 ->
    C - $0;
hint(C) when $A =< C, C =< $F ->
    C - $A + 10;
hint(C) when $a =< C, C =< $f ->
    C - $a + 10.

to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

list_to_hexstr([]) ->
    [];
list_to_hexstr([H|T]) ->
    to_hex(H) ++ list_to_hexstr(T).

hexstr_to_list([X,Y|T]) ->
    [hint(X)*16 + hint(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
    [].

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
deserialize_boolean_little_test() -> % 1b
  ?assertEqual({true, <<>>}, deserialize(hexstr_to_bin("010000000a000000ff01"))).
% no null value specified
serialize_boolean_little_test() -> % 1b
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_boolean(true))), "010000000a000000ff01").

deserialize_guid_little_test() -> % 0a369037-75d3-b24d-6721-5a1d44d4bed5
  ?assertEqual({<<10, 54, 144, 55, 117, 211, 178, 77, 103, 33, 90, 29, 68, 212, 190, 213>>, <<>>}, deserialize(hexstr_to_bin("0100000019000000fe0a36903775d3b24d67215a1d44d4bed5"))).
deserialize_guid_null_little_test() -> % 0Ng
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("0100000019000000fe00000000000000000000000000000000"))).
serialize_guid_little_test() -> % 0a369037-75d3-b24d-6721-5a1d44d4bed5
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_guid(<<10, 54, 144, 55, 117, 211, 178, 77, 103, 33, 90, 29, 68, 212, 190, 213>>))), "0100000019000000fe0a36903775d3b24d67215a1d44d4bed5").
serialize_guid_null_little_test() -> % 0Ng
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_guid(null))), "0100000019000000fe00000000000000000000000000000000").

deserialize_byte_little_test() -> % 0x01
  ?assertEqual({<<1>>, <<>>}, deserialize(hexstr_to_bin("010000000a000000fc01"))).
% no null value specified
serialize_byte_little_test() -> % 0x01
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_byte(<<1>>))), "010000000a000000fc01").

deserialize_short_little_test() -> % 1h
  ?assertEqual({1, <<>>}, deserialize(hexstr_to_bin("010000000b000000fb0100"))).
deserialize_short_null_little_test() -> % 0Nh
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("010000000b000000fb0080"))).
serialize_short_little_test() -> % 1h
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_short(1))), "010000000b000000fb0100").
serialize_short_null_little_test() -> % 0Nh
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_short(null))), "010000000b000000fb0080").

deserialize_integer_little_test() -> % 1i
  ?assertEqual({1, <<>>}, deserialize(hexstr_to_bin("010000000d000000fa01000000"))).
deserialize_integer_null_little_test() -> % 0Ni
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("010000000d000000fa00000080"))).
serialize_integer_little_test() -> % 1i
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_int(1))), "010000000d000000fa01000000").
serialize_integer_null_little_test() -> % 0Ni
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_int(null))), "010000000d000000fa00000080").

deserialize_long_little_test() -> % 1j
  ?assertEqual({1, <<>>}, deserialize(hexstr_to_bin("0100000011000000f90100000000000000"))).
deserialize_long_null_little_test() -> % 0Nj
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("0100000011000000f90000000000000080"))).
serialize_long_little_test() -> % 1j
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_long(1))), "0100000011000000f90100000000000000").
serialize_long_null_little_test() -> % 0Nj
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_long(null))), "0100000011000000f90000000000000080").

deserialize_real_little_test() -> % 1.0e
  ?assertEqual({1.0, <<>>}, deserialize(hexstr_to_bin("010000000d000000f80000803f"))).
deserialize_real_null_little_test() -> % 0Ne
   ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("010000000d000000f80000c0ff"))).
serialize_real_little_test() -> % 1.0e
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_real(1.0))), "010000000d000000f80000803f").
serialize_real_null_little_test() -> % 0Ne
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_real(null))), "010000000d000000f80000c0ff").

deserialize_float_little_test() -> % 1.0f
  ?assertEqual({1.0, <<>>}, deserialize(hexstr_to_bin("0100000011000000f7000000000000f03f"))).
deserialize_float_null_little_test() -> % 0Nf
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("0100000011000000f7000000000000f8ff"))).
serialize_float_little_test() -> % 1.0f
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_float(1.0))), "0100000011000000f7000000000000f03f").
serialize_float_null_little_test() -> % 0Nf
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_float(null))), "0100000011000000f7000000000000f8ff").

deserialize_char_little_test() -> % "a"
  ?assertEqual({<<"a">>, <<>>}, deserialize(hexstr_to_bin("010000000a000000f661"))).
deserialize_char_null_little_test() -> % " "
  ?assertEqual({<<" ">>, <<>>}, deserialize(hexstr_to_bin("010000000a000000f620"))). % no null value specified
serialize_char_little_test() -> % "a"
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_char(<<"a">>))), "010000000a000000f661").
serialize_char_null_little_test() -> % " "
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_char(null))), "010000000a000000f620"). % no null value specified

deserialize_symbol_length1_little_test() -> % `a
  ?assertEqual({<<"a">>, <<>>}, deserialize(hexstr_to_bin("010000000b000000f56100"))).
deserialize_symbol_null_little_test() -> % `
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("010000000a000000f500"))).
deserialize_symbol_length2_little_test() -> % `ab
  ?assertEqual({<<"ab">>, <<>>}, deserialize(hexstr_to_bin("010000000c000000f5616200"))).
deserialize_symbol_length3_little_test() -> % `abc
  ?assertEqual({<<"abc">>, <<>>}, deserialize(hexstr_to_bin("010000000d000000f561626300"))).
deserialize_symbol_length4_little_test() -> % `abcd
  ?assertEqual({<<"abcd">>, <<>>}, deserialize(hexstr_to_bin("010000000e000000f56162636400"))).
deserialize_symbol_length5_little_test() -> % `abcde
  ?assertEqual({<<"abcde">>, <<>>}, deserialize(hexstr_to_bin("010000000f000000f5616263646500"))).
serialize_symbol_length1_little_test() -> % `a
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbol(<<"a">>))), "010000000b000000f56100"),
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbol(a))), "010000000b000000f56100").
serialize_symbol_null_little_test() -> % `
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbol(null))), "010000000a000000f500").
serialize_symbol_length2_little_test() -> % `ab
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbol(<<"ab">>))), "010000000c000000f5616200"),
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbol(ab))), "010000000c000000f5616200").
serialize_symbol_length3_little_test() -> % `abc
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbol(<<"abc">>))), "010000000d000000f561626300"),
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbol(abc))), "010000000d000000f561626300").
serialize_symbol_length4_little_test() -> % `abcd
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbol(<<"abcd">>))), "010000000e000000f56162636400"),
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbol(abcd))), "010000000e000000f56162636400").
serialize_symbol_length5_little_test() -> % `abcde
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbol(<<"abcde">>))), "010000000f000000f5616263646500"),
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbol(abcde))), "010000000f000000f5616263646500").

deserialize_timestamp_little_test() -> % 2014.06.23D11:34:39.412547000
  ?assertEqual({456838479412547000, <<>>}, deserialize(hexstr_to_bin("0100000011000000f4b84d1d352d045706"))).
deserialize_timestamp_null_little_test() -> % 0Np
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("0100000011000000f40000000000000080"))).
serialize_timestamp_little_test() -> % 2014.06.23D11:34:39.412547000
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_timestamp(456838479412547000))), "0100000011000000f4b84d1d352d045706").
serialize_timestamp_null_little_test() -> % 0Np
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_timestamp(null))), "0100000011000000f40000000000000080").

deserialize_month_201401_little_test() -> % 2014.01m
  ?assertEqual({168, <<>>}, deserialize(hexstr_to_bin("010000000d000000f3a8000000"))).
deserialize_month_null_little_test() -> % 0Nm
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("010000000d000000f300000080"))).
deserialize_month_199501_little_test() -> % 1995.01m
  ?assertEqual({-60, <<>>}, deserialize(hexstr_to_bin("010000000d000000f3c4ffffff"))).
serialize_month_201401_little_test() -> % 2014.01m
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_month(168))), "010000000d000000f3a8000000").
serialize_month_null_little_test() -> % 0Nm
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_month(null))), "010000000d000000f300000080").
serialize_month_199501_little_test() -> % 1995.01m
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_month(-60))), "010000000d000000f3c4ffffff").

deserialize_date_20140101_little_test() -> % 2014.01.01
  ?assertEqual({5114, <<>>}, deserialize(hexstr_to_bin("010000000d000000f2fa130000"))).
deserialize_date_null_little_test() -> % 0Nd
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("010000000d000000f200000080"))).
deserialize_date_19950101_little_test() -> % 1995.01.01
  ?assertEqual({-1826, <<>>}, deserialize(hexstr_to_bin("010000000d000000f2def8ffff"))).
serialize_date_20140101_little_test() -> % 2014.01.01
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_date(5114))), "010000000d000000f2fa130000").
serialize_date_null_little_test() -> % 0Nd
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_date(null))), "010000000d000000f200000080").
serialize_date_19950101_little_test() -> % 1995.01.01
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_date(-1826))), "010000000d000000f2def8ffff").

deserialize_datetime_little_test() -> % 2014.06.23T11:49:31.533
  ?assertEqual({4662535674435194874, <<>>}, deserialize(hexstr_to_bin("0100000011000000f1facf4b237ea7b440"))).
deserialize_datetime_null_little_test() -> % 0Nz
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("0100000011000000f1000000000000f8ff"))).
serialize_datetime_little_test() -> % 2014.06.23T11:49:31.533
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_datetime(4662535674435194874))), "0100000011000000f1facf4b237ea7b440").
serialize_datetime_null_little_test() -> % 0Nz
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_datetime(null))), "0100000011000000f1000000000000f8ff").

deserialize_timespan_little_test() -> % 00:01:00.000000000
  ?assertEqual({60000000000, <<>>}, deserialize(hexstr_to_bin("0100000011000000f0005847f80d000000"))).
deserialize_timespan_null_little_test() -> % 0Nn
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("0100000011000000f00000000000000080"))).
serialize_timespan_little_test() -> % 00:01:00.000000000
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_timespan(60000000000))), "0100000011000000f0005847f80d000000").
serialize_timespan_null_little_test() -> % 0Nn
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_timespan(null))), "0100000011000000f00000000000000080").

deserialize_minute_little_test() -> % 00:01
  ?assertEqual({1, <<>>}, deserialize(hexstr_to_bin("010000000d000000ef01000000"))).
deserialize_minute_null_little_test() -> % 0Nu
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("010000000d000000ef00000080"))).
serialize_minute_little_test() -> % 00:01
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_minute(1))), "010000000d000000ef01000000").
serialize_minute_null_little_test() -> % 0Nu
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_minute(null))), "010000000d000000ef00000080").

deserialize_second_little_test() -> % 00:00:01
  ?assertEqual({1, <<>>}, deserialize(hexstr_to_bin("010000000d000000ee01000000"))).
deserialize_second_null_little_test() -> % 0Nv
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("010000000d000000ee00000080"))).
serialize_second_little_test() -> % 00:00:01
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_second(1))), "010000000d000000ee01000000").
serialize_second_null_little_test() -> % 0Nv
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_second(null))), "010000000d000000ee00000080").

deserialize_time_little_test() -> % 00:00:00.001
  ?assertEqual({1, <<>>}, deserialize(hexstr_to_bin("010000000d000000ed01000000"))).
deserialize_time_null_little_test() -> % 0Nt
  ?assertEqual({null, <<>>}, deserialize(hexstr_to_bin("010000000d000000ed00000080"))).
serialize_time_little_test() -> % 00:00:00.001
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_time(1))), "010000000d000000ed01000000").
serialize_time_null_little_test() -> % 0Nt
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_time(null))), "010000000d000000ed00000080").

deserialize_boolean_vector_little_test() ->
  ?assertEqual({[true, false], <<>>}, deserialize(hexstr_to_bin("01000000100000000100020000000100"))).
serialize_boolean_vector_little_test() ->
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_booleans([true, false]))), "01000000100000000100020000000100").

deserialize_guid_vector_little_test() ->
  ?assertEqual({[<<10, 54, 144, 55, 117, 211, 178, 77, 103, 33, 90, 29, 68, 212, 190, 213>>, <<10, 54, 144, 55, 117, 211, 178, 77, 103, 33, 90, 29, 68, 212, 190, 213>>], <<>>}, deserialize(hexstr_to_bin("010000002e0000000200020000000a36903775d3b24d67215a1d44d4bed50a36903775d3b24d67215a1d44d4bed5"))).
serialize_guid_vector_little_test() ->
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_guids([<<10, 54, 144, 55, 117, 211, 178, 77, 103, 33, 90, 29, 68, 212, 190, 213>>, <<10, 54, 144, 55, 117, 211, 178, 77, 103, 33, 90, 29, 68, 212, 190, 213>>]))), "010000002e0000000200020000000a36903775d3b24d67215a1d44d4bed50a36903775d3b24d67215a1d44d4bed5").

deserialize_byte_vector_little_test() ->
  ?assertEqual({[<<0>>, <<1>>, <<2>>, <<3>>, <<4>>], <<>>}, deserialize(hexstr_to_bin("01000000130000000400050000000001020304"))).
serialize_byte_vector_little_test() ->
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_bytes([<<0>>, <<1>>, <<2>>, <<3>>, <<4>>]))), "01000000130000000400050000000001020304").

deserialize_short_vector_little_test() ->
  ?assertEqual({[1, 2, 3], <<>>}, deserialize(hexstr_to_bin("0100000014000000050003000000010002000300"))).
serialize_short_vector_little_test() ->
    ?assertEqual(bin_to_hexstr(serialize(async, serialize_shorts([1, 2, 3]))), "0100000014000000050003000000010002000300").

deserialize_integer_vector_1item_little_test() ->
  ?assertEqual({[1], <<>>}, deserialize(hexstr_to_bin("010000001200000006000100000001000000"))).
deserialize_integer_vector_little_test() ->
  ?assertEqual({[1, 2, 3], <<>>}, deserialize(hexstr_to_bin("010000001a000000060003000000010000000200000003000000"))).
serialize_integer_vector_1item_little_test() ->
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_ints([1]))), "010000001200000006000100000001000000").
serialize_integer_vector_little_test() ->
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_ints([1, 2, 3]))), "010000001a000000060003000000010000000200000003000000").

deserialize_long_vector_little_test() ->
  ?assertEqual({[1, 2, 3], <<>>}, deserialize(hexstr_to_bin("0100000026000000070003000000010000000000000002000000000000000300000000000000"))).
serialize_long_vector_little_test() ->
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_longs([1, 2, 3]))), "0100000026000000070003000000010000000000000002000000000000000300000000000000").

deserialize_real_vector_little_test() ->
  ?assertEqual({[1.0, 2.0, 3.0], <<>>}, deserialize(hexstr_to_bin("010000001a0000000800030000000000803f0000004000004040"))).
serialize_real_vector_little_test() ->
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_reals([1.0, 2.0, 3.0]))), "010000001a0000000800030000000000803f0000004000004040").

deserialize_float_vector_little_test() ->
  ?assertEqual({[1.0, 2.0, 3.0], <<>>}, deserialize(hexstr_to_bin("0100000026000000090003000000000000000000f03f00000000000000400000000000000840"))).
serialize_float_vector_little_test() ->
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_floats([1.0, 2.0, 3.0]))), "0100000026000000090003000000000000000000f03f00000000000000400000000000000840").

deserialize_char_vector_little_test() ->
  ?assertEqual({[<<"a">>, <<"b">>, <<"c">>], <<>>}, deserialize(hexstr_to_bin("01000000110000000a0003000000616263"))).
serialize_char_vector_little_test() ->
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_chars([<<"a">>, <<"b">>, <<"c">>]))), "01000000110000000a0003000000616263").
serialize_string_little_test() ->
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_string(<<"abc">>))), "01000000110000000a0003000000616263").

deserialize_symbol_vector_little_test() ->
  ?assertEqual({[<<"a">>, <<"ab">>, <<"abc">>], <<>>}, deserialize(hexstr_to_bin("01000000170000000b0003000000610061620061626300"))).
serialize_symbol_vector_little_test() ->
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_symbols([a, ab, abc]))), "01000000170000000b0003000000610061620061626300").

deserialize_timestamp_vector_little_test() -> % (2014.01.01D12:00:00.000000000;2014.01.02D12:00:00.000000000;2014.01.03D12:00:00.000000000)
  ?assertEqual({[441892800000000000, 441979200000000000, 442065600000000000], <<>>}, deserialize(hexstr_to_bin("01000000260000000c00030000000080cd0c29eb210600801c9ebd39220600806b2f52882206"))).
serialize_timestamp_vector_little_test() -> % (2014.01.01D12:00:00.000000000;2014.01.02D12:00:00.000000000;2014.01.03D12:00:00.000000000)
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_timestamps([441892800000000000, 441979200000000000, 442065600000000000]))), "01000000260000000c00030000000080cd0c29eb210600801c9ebd39220600806b2f52882206").

deserialize_month_vector_little_test() -> % (1995.01m;1995.02m;1995.03m)
  ?assertEqual({[-60,-59, -58], <<>>}, deserialize(hexstr_to_bin("010000001a0000000d0003000000c4ffffffc5ffffffc6ffffff"))).
serialize_month_vector_little_test() -> % (1995.01m;1995.02m;1995.03m)
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_months([-60,-59, -58]))), "010000001a0000000d0003000000c4ffffffc5ffffffc6ffffff").

deserialize_date_vector_little_test() -> % (2014.01.01;2014.01.02;2014.01.03)
  ?assertEqual({[5114, 5115, 5116], <<>>}, deserialize(hexstr_to_bin("010000001a0000000e0003000000fa130000fb130000fc130000"))).
serialize_date_vector_little_test() -> % (2014.01.01;2014.01.02;2014.01.03)
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_dates([5114, 5115, 5116]))), "010000001a0000000e0003000000fa130000fb130000fc130000").

deserialize_datetime_vector_little_test() -> % (2014.06.23T11:49:31.533;2014.06.23T11:49:31.534;2014.06.23T11:49:31.535)
  ?assertEqual({[4662535674435194874,4662535674435207600, 4662535674435220326], <<>>}, deserialize(hexstr_to_bin("01000000260000000f0003000000facf4b237ea7b440b0014c237ea7b44066334c237ea7b440"))).
serialize_datetime_vector_little_test() -> % (2014.06.23T11:49:31.533;2014.06.23T11:49:31.534;2014.06.23T11:49:31.535)
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_datetimes([4662535674435194874,4662535674435207600, 4662535674435220326]))), "01000000260000000f0003000000facf4b237ea7b440b0014c237ea7b44066334c237ea7b440").

deserialize_timespan_vector_little_test() -> % (00:01:00.000000000;00:02:00.000000000;00:03:00.000000000)
  ?assertEqual({[60000000000, 120000000000, 180000000000], <<>>}, deserialize(hexstr_to_bin("0100000026000000100003000000005847f80d00000000b08ef01b0000000008d6e829000000"))).
serialize_timespan_vector_little_test() -> % (00:01:00.000000000;00:02:00.000000000;00:03:00.000000000)
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_timespans([60000000000, 120000000000, 180000000000]))), "0100000026000000100003000000005847f80d00000000b08ef01b0000000008d6e829000000").

deserialize_minute_vector_little_test() -> % (00:01;00:02;00:03)
  ?assertEqual({[1, 2, 3], <<>>}, deserialize(hexstr_to_bin("010000001a000000110003000000010000000200000003000000"))).
serialize_minute_vector_little_test() -> % (00:01;00:02;00:03)
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_minutes([1, 2, 3]))), "010000001a000000110003000000010000000200000003000000").

deserialize_second_vector_little_test() -> % (00:00:01;00:00:02;00:00:03)
  ?assertEqual({[1, 2, 3], <<>>}, deserialize(hexstr_to_bin("010000001a000000120003000000010000000200000003000000"))).
serialize_second_vector_little_test() -> % (00:00:01;00:00:02;00:00:03)
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_seconds([1, 2, 3]))), "010000001a000000120003000000010000000200000003000000").

deserialize_time_vector_little_test() -> % (00:00:00.001;00:00:00.002;00:00:00.003)
  ?assertEqual({[1, 2, 3], <<>>}, deserialize(hexstr_to_bin("010000001a000000130003000000010000000200000003000000"))).
serialize_time_vector_little_test() -> % (00:00:00.001;00:00:00.002;00:00:00.003)
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_times([1, 2, 3]))), "010000001a000000130003000000010000000200000003000000").

deserialize_generallist_with_vectors_little_test() -> % `byte$enlist til 5
  ?assertEqual({[[<<0>>, <<1>>, <<2>>, <<3>>, <<4>>]], <<>>}, deserialize(hexstr_to_bin("01000000190000000000010000000400050000000001020304"))).
deserialize_generallist_with_atoms_little_test() -> % (1l;1b;`a)
  ?assertEqual({[1, true, <<"a">>], <<>>}, deserialize(hexstr_to_bin("010000001c000000000003000000f90100000000000000ff01f56100"))).
serialize_generallist_with_vectors_little_test() -> % `byte$enlist til 5
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_generallist([serialize_bytes([<<0>>, <<1>>, <<2>>, <<3>>, <<4>>])]))), "01000000190000000000010000000400050000000001020304").
serialize_generallist_with_atoms_little_test() -> % (1l;1b;`a)
  ?assertEqual(bin_to_hexstr(serialize(async, serialize_generallist([serialize_long(1), serialize_boolean(true), serialize_symbol(a)]))), "010000001c000000000003000000f90100000000000000ff01f56100").

deserialize_dictionary_with_atom_values_little_test() -> % (`a`b)!(2 3)
  ?assertEqual({[{<<"a">>, 2}, {<<"b">>, 3}], <<>>}, deserialize(hexstr_to_bin("0100000021000000630b0002000000610062000600020000000200000003000000"))).
deserialize_dictionary_with_atom_values2_little_test() -> % (`abc`def)!(1 2)
  ?assertEqual({[{<<"abc">>, 1}, {<<"def">>, 2}], <<>>}, deserialize(hexstr_to_bin("010000002d000000630b0002000000616263006465660007000200000001000000000000000200000000000000"))).

deserialize_sorted_dictionary_with_atom_values_little_test() ->
  ?assertEqual({[{<<"a">>, 2}, {<<"b">>, 3}], <<>>}, deserialize(hexstr_to_bin("01000000210000007f0b0102000000610062000600020000000200000003000000"))).

deserialize_dictionary_with_vector_values_little_test() ->
  ?assertEqual({[{<<"a">>, [2]}, {<<"b">>, [3]}], <<>>}, deserialize(hexstr_to_bin("010000002d000000630b0002000000610062000000020000000600010000000200000006000100000003000000"))).

deserialize_table_little_test() ->
  ?assertEqual({[{<<"a">>, [2]}, {<<"b">>, [3]}], <<>>}, deserialize(hexstr_to_bin("010000002f0000006200630b0002000000610062000000020000000600010000000200000006000100000003000000"))).

deserialize_sorted_table_little_test() ->
  ?assertEqual({[{<<"a">>, [2]}, {<<"b">>, [3]}], <<>>}, deserialize(hexstr_to_bin("010000002f0000006201630b0002000000610062000000020000000603010000000200000006000100000003000000"))).

deserialize_keyed_table_little_test() ->
  ?assertEqual({[{{<<"a">>,[2]},{<<"b">>,[3]}}], <<>>}, deserialize(hexstr_to_bin("010000003f000000636200630b00010000006100000001000000060001000000020000006200630b0001000000620000000100000006000100000003000000"))).

deserialize_sorted_keyed_table_little_test() ->
  ?assertEqual({[{{<<"a">>,[2]},{<<"b">>,[3]}}], <<>>}, deserialize(hexstr_to_bin("010000003f0000007f6201630b00010000006100000001000000060001000000020000006200630b0001000000620000000100000006000100000003000000"))).

deserialize_error_little_test() ->
  ?assertEqual({{error, <<"type">>}, <<>>}, deserialize(hexstr_to_bin("010200000e000000807479706500"))).
