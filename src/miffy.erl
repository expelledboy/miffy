-module(miffy).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([ encode/1, encode/2 ]).
-export([ decode/1, decode/2 ]).

%% --

-spec encode(map()) -> binary().
encode(EJson) ->
    encode(EJson, #{}).

-spec encode(map(), map()) -> binary().
encode(EJson, Types) when is_map(EJson), is_map(Types) ->
    Data = translate(fun compile/4, EJson,Types),
    jiffy:encode(Data).

%% --

-spec decode(iolist() | binary()) -> map().
decode(Json) ->
    decode(Json, #{}).

-spec decode(iolist() | binary(), map()) -> map().
decode(Json, Types) ->
    Data = jiffy:decode(Json, [return_maps]),
    translate(fun reduce/4, Data, Types).

%% --

-spec translate(fun(), map(), map()) -> map().
translate(Fun, Map, Types) when is_map(Map); is_map(Types) ->
    maps:fold(fun(K,V,D) -> Fun(K,V,D,Types) end, #{}, Map).

%% --

-spec compile(atom(), any(), map(), map()) -> map().
compile(Key,V,D,Ts) when is_atom(Key),
                         is_map(V) ->
    Types = maps:get(Key, Ts, #{}),
    Value = translate(fun compile/4, V, Types),
    maps:put(Key, Value, D);
compile(Key,V,D,Ts) when is_atom(Key) ->
    Value = convert(V,maps:get(Key, Ts, nil)),
    maps:put(Key, Value, D).

%% --

-spec reduce(binary(), any(), map(), map()) -> map().
reduce(K,V,D,Ts) when is_binary(K),
                      is_map(V) ->
    Key = binary_to_atom(K, latin1),
    Types = maps:get(Key, Ts, #{}),
    Value = translate(fun reduce/4, V, Types),
    maps:put(Key, Value, D);
reduce(K,V,D,Ts) when is_binary(K) ->
    Key = binary_to_atom(K, latin1),
    Value = convert(V,maps:get(Key, Ts, nil)),
    maps:put(Key, Value, D).

%% --

-spec convert(binary() | integer(), atom | nil) -> map().
convert(V, string) when is_binary(V) ->
    binary_to_list(V);
convert(V, string) when is_list(V) ->
    list_to_binary(V);
convert(V, atom) when is_atom(V) ->
    V;
convert(V, atom) when is_binary(V) ->
    binary_to_atom(V, latin1);
convert(V, nil) when is_binary(V) ->
    binary_to_list(V);
convert(V, nil) ->
    V.
