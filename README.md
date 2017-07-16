Miffy
=====

[![Travis](https://img.shields.io/travis/expelledboy/miffy.svg)](https://travis-ci.org/expelledboy/miffy)
[![Hex.pm](https://img.shields.io/hexpm/v/miffy.svg)](https://hex.pm/packages/miffy)
[![Hex.pm](https://img.shields.io/hexpm/dt/miffy.svg)](https://hex.pm/packages/miffy)

Jiffy wrapper which returns pretty maps.

### Usage

Optional data types can be passed as a second parameter, to provide seemless translation.

```erlang
Types = #{ atom => atom,
	   string => string,
	   obj => #{ sub => atom } },
Erlang = #{ atom => value,
	    list => ["value"],
	    string => "value",
	    bool => true,
	    integer => 1,
	    float => 1.0,
	    obj => #{ sub => value } },
Json = miffy:encode(Erlang, Types),
AtomStr = '{"string":"value","obj":{"sub":"value"},"list":[[118,97,108,117,101]],"integer":1,"float":1.0,"bool":true,"atom":"value"}',
Json = erlang:atom_to_binary(AtomStr, latin1), %% HACK dont do this to create strings
Erlang = miffy:decode(Json, Types).
```

### Note

Miffy does NOT attempt to translate lists deeply, see unit tests for example behaviour.
