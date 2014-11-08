[![Build Status](https://secure.travis-ci.org/cinovo/erlang-q.png)](http://travis-ci.org/cinovo/erlang-q)

# Q interfacing with Erlang

## Usage

If you use [rebar](https://github.com/rebar/rebar) just add

{deps, [
	{q, ".*", {git, "git://github.com/cinovo/q.git", {tag, "v0.1.1"}}},
]}.

to your dependencies.

### execute Q code

	{ok, Pid}=q:connect(<<"localhost">>, 5010).
	2=q:execute(Pid, <<"1+1">>).
	q.close(Pid).

### execute Q functions

	{ok, Pid} = connect(<<"localhost">>, 5010),
	15=q:execute(Pid, <<"sum">>, q_ipcp:serialize_ints([1, 2, 3, 4, 5])).

### subscribe (kdb+tick)

After you subscribe incoming data is distributed using the gen_event behaviour.
Events are {q, Table, Data}

	{ok, Pid} = q:connect({q_demo_handler, []}, <<"localhost">>, 5010).
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

TODO

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

TODO

### minute

TODO

### second

TODO

### time

TODO

### mixed list

#### Q => Erlang

	(1j; 1b; `a) => [1, true, a]
	() => []

#### Erlang => Q

	q_ipcp:serialize_generallist([q_ipcp:serialize_long(1), q_ipcp:serialize_boolean(true), q_ipcp:serialize_symbol(a)])) => (1j; 1b; `a)

### lists

#### Q => Erlang

The items of a list are serialized like described in the type.

	(1i;2i;3i) => [1, 2, 3]

#### Erlang => Q

Each `q_ipcp:serialize_TYPE(VALUE)` has a counterpart for lists  `q_ipcp:serialize_TYPEs([VALUE1, VALUE2])`

	q_ipcp:serialize_floats([1.0, 2.0, 3.0]) => (1.0f; 2.0f; 3.0f)

### dict

TODO

### table

TODO

## Development

### Compile and test

	make all test

### Generate documentation

	make doc

## What is missing?

* decompression of received bytes if they are compressed
