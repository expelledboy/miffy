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
    Data = translate(EJson,Types),
    jiffy:encode(Data).

%% --

-spec decode(iolist() | binary()) -> map().
decode(Json) ->
    decode(Json, #{}).

-spec decode(iolist() | binary(), map()) -> map().
decode(Json, Types) ->
    Data = jiffy:decode(Json, [return_maps]),
    translate(Data, Types).

%% --

-spec translate(map(), map()) -> map().
translate(Map, Types) when is_map(Map); is_map(Types) ->
    maps:fold(fun(K,V,D) ->
                      Key = ensure_key(K),
                      Ts = maps:get(Key, Types, nil),
                      Value = convert(V,Ts),
                      maps:put(Key, Value, D)
              end, #{}, Map).

-spec ensure_key(binary() | atom()) -> atom().
ensure_key(K) when is_binary(K) -> binary_to_atom(K, latin1);
ensure_key(K) when is_atom(K) -> K.

%% --

convert(Vs, {collection,Ts}) when is_list(Vs)   -> collection([],Vs,Ts);
convert(V, Ts) when is_map(V), is_map(Ts)       -> translate(V,Ts);
convert(V, nil) when is_map(V)                  -> translate(V,#{});
convert(V, string) when is_binary(V)            -> binary_to_list(V);
convert(V, string) when is_list(V)              -> list_to_binary(V);
convert(V, atom) when is_atom(V)                -> V;
convert(V, atom) when is_binary(V)              -> binary_to_atom(V, latin1);
convert(V, nil) when is_binary(V)               -> binary_to_list(V);
convert(V, binary) when is_binary(V)            -> V;
convert(V, nil) when is_list(V)                 -> V;
convert(V, nil) when is_atom(V)                 -> V;
convert(V, nil) when is_integer(V)              -> V;
convert(V, nil) when is_float(V)                -> V.

collection(Collection, [], _Ts) ->
    Collection;
collection(Acc,[V|Vs], Ts) ->
    Value = convert(V,Ts),
    collection([Value|Acc],Vs,Ts).
