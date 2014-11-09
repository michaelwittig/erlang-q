[![Build Status](https://secure.travis-ci.org/cinovo/erlang-q.png)](http://travis-ci.org/cinovo/erlang-q)

# Q interfacing with Erlang

## Usage

If you use [rebar](https://github.com/rebar/rebar) just add

	{deps, [
		{q, ".*", {git, "git://github.com/cinovo/erlang-q.git", {tag, "v0.1.2"}}}
	]}.

to your dependencies.

### execute Q code as string

	{ok, Pid} = q:connect(<<"localhost">>, 5000).
	2 = q:execute(Pid, <<"1+1">>).
	q:close(Pid).

### execute Q functions with params

	{ok, Pid} = q:connect(<<"localhost">>, 5000).
	15 = q:execute(Pid, <<"sum">>, q_ipcp:serialize_ints([1, 2, 3, 4, 5])).
	q:close(Pid).

### subscribe (kdb+tick)

After you subscribe, incoming data is distributed using the [gen_event](http://www.erlang.org/doc/man/gen_event.html) behaviour.
Events are `{q, Table, Data}`. If you want to print the data to sysour you can use [q_demo_handler](https://github.com/cinovo/erlang-q/blob/master/src/q_demo_handler.erl) module.

	{ok, Pid} = q:connect({q_demo_handler, []}, <<"localhost">>, 5000).
	q:subscribe(Pid, <<"trade">>, <<"TEST">>).

## (De)Serialization

Keep in mind that null values in Q are deserialized to atom null in Erlang.

### boolean

#### Q => Erlang

	1b => true
	0b => false

#### Erlang => Q

	q_ipcp:serialize_boolean(true) => 1b
	q_ipcp:serialize_boolean(false) => 0b

### guid

#### Q => Erlang

	0a369037-75d3-b24d-6721-5a1d44d4bed5 => <<10, 54, 144, 55, 117, 211, 178, 77, 103, 33, 90, 29, 68, 212, 190, 213>>
	0Ng => null

#### Erlang => Q

	q_ipcp:serialize_guid(<<10, 54, 144, 55, 117, 211, 178, 77, 103, 33, 90, 29, 68, 212, 190, 213>>) => 0a369037-75d3-b24d-6721-5a1d44d4bed5
	q_ipcp:serialize_guid(null) => 0Ng

### byte

#### Q => Erlang

	0x01 => <<1>>

#### Erlang => Q

	q_ipcp:serialize_byte(<<1>>) => 0x01

### short

#### Q => Erlang

	1h => 1
	0Nh => null

#### Erlang => Q

	q_ipcp:serialize_short(1) => 1h
	q_ipcp:serialize_short(null) => 0Nh

### int

#### Q => Erlang

	1i => 1
	0Ni => null

#### Erlang => Q

	q_ipcp:serialize_int(1) => 1i
	q_ipcp:serialize_int(null) => 0Ni

### long

#### Q => Erlang

	1j => 1
	0Nj => null

#### Erlang => Q

	q_ipcp:serialize_long(1) => 1j
	q_ipcp:serialize_long(null) => 0Nj

### real

#### Q => Erlang

	1.0e => 1.0
	0Ne => null

#### Erlang => Q

	q_ipcp:serialize_real(1.0) => 1.0e
	q_ipcp:serialize_long(null) => 0Ne

### float

#### Q => Erlang

	1.0f => 1.0
	0Nf => null

#### Erlang => Q

	q_ipcp:serialize_float(1.0) => 1.0f
	q_ipcp:serialize_float(null) => 0Nf

### char

#### Q => Erlang

	"a" => <<"a">>

#### Erlang => Q

	q_ipcp:serialize_char(<<"a">>) => "a"

### symbol

#### Q => Erlang

	`a => <<"a">>
	` => null

#### Erlang => Q

	q_ipcp:serialize_symbol(a) => `a
	q_ipcp:serialize_symbol(<<"a">>) => `a
	q_ipcp:serialize_symbol(null) => `

#### timestamp

#### Q => Erlang

	2014.06.23D11:34:39.412547000 => 456838479412547000
	0Np => null

#### Erlang => Q

	456838479412547000 => 014.06.23D11:34:39.412547000
	null => 0Np

### month

#### Q => Erlang

	2014.01m => 168 % months since 2000.01
	0Nm => null

#### Erlang => Q

NOT YET SUPPORTED

### date

#### Q => Erlang

	2014.01.01 => 5114 % days since 2000.01.01
	0Nd => null

#### Erlang => Q

NOT YET SUPPORTED

### datetime

#### Q => Erlang

	2014.06.23T11:49:31.533 => 4662535674435194874
	0Nz => null

#### Erlang => Q

NOT YET SUPPORTED

### timespan

#### Q => Erlang

	00:01:00.000000000 => 60000000000
	0Nn => null

#### Erlang => Q

NOT YET SUPPORTED

### minute

#### Q => Erlang

	00:01 => 1
	0Nu => null

#### Erlang => Q

NOT YET SUPPORTED

### second

#### Q => Erlang

	00:00:01 => 1
	0Nv => null

#### Erlang => Q

NOT YET SUPPORTED

### time

#### Q => Erlang 

	00:00:00.001 => 1
	0Nt => null

#### Erlang => Q

NOT YET SUPPORTED

### mixed list

#### Q => Erlang

	(1j; 1b; `a) => [1, true, a]
	() => []

#### Erlang => Q

	q_ipcp:serialize_generallist([q_ipcp:serialize_long(1), q_ipcp:serialize_boolean(true), q_ipcp:serialize_symbol(a)])) => (1j; 1b; `a)

### lists

#### Q => Erlang

The items of a list are deserialized like described in the type.

	(1i;2i;3i) => [1, 2, 3]

#### Erlang => Q

Each `q_ipcp:serialize_TYPE(VALUE)` has a counterpart for lists  `q_ipcp:serialize_TYPEs([VALUE1, VALUE2])`

	q_ipcp:serialize_floats([1.0, 2.0, 3.0]) => (1.0f; 2.0f; 3.0f)

### dict

Each key value pair is represented as a tuple `{Key, Value}`. The keys are values of a dict are deserialized like described in the type.

#### Q => Erlang

	(`a`b)!(2 3) => [{<<"a">>, 2}, {<<"b">>, 3}]

#### Erlang => Q

NOT YET SUPPORTED

### table

To handle tables in Erlang you should use [q_table](https://github.com/cinovo/erlang-q/blob/master/src/q_table.erl) module.

#### Q => Erlang

	([] a:enlist 2; b:enlist 3) => [{<<"a">>, [2]}, {<<"b">>, [3]}]

#### Erlang => Q

NOT YET SUPPORTED

## Development

### Run tests

	make test

### Generate documentation

	make doc

## EDoc

[click me](http://htmlpreview.github.io/?https://github.com/cinovo/erlang-q/blob/master/doc/index.html)

## What is missing?

* time types helper
* decompression of received bytes if they are compressed
* simple Erlang (de)serialization
