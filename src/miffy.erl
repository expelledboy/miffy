-module(miffy).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([ encode/1, encode/2 ]).
-export([ decode/1, decode/2 ]).

%% --

-spec encode(map()) -> binary().
encode(EJson) ->
    encode(EJson, nil).

-spec encode(map(), map() | nil) -> binary().
encode(EJson, Types) when is_map(EJson) ->
    Data = translate(EJson,Types),
    Parser = miffy_utils:parser(),
    Parser:encode(Data).

%% --

-spec decode(iolist() | binary()) -> map().
decode(Json) ->
    decode(Json, nil).

-spec decode(iolist() | binary(), map() | nil) -> map().
decode(Json, Types) ->
    Parser = miffy_utils:parser(),
    Data = Parser:decode(Json, [return_maps]),
    translate(Data, Types).

%% --

-spec translate(term(), term()) -> term().
translate(Obj, Types) when is_map(Obj), is_map(Types) ->
    maps:fold(fun(K,V,D) ->
                      Key = ensure_key(K),
                      Ts = maps:get(Key, Types, nil),
                      Value = translate(V,Ts),
                      maps:put(Key, Value, D)
              end, #{}, Obj);
translate(Obj, nil) when is_map(Obj) ->
    translate(Obj, #{});
translate(Value, Type) when not is_map(Value), not is_map(Type) ->
    convert(Value,Type).

-spec ensure_key(binary() | atom()) -> atom().
ensure_key(K) when is_binary(K) -> binary_to_atom(K, latin1);
ensure_key(K) when is_atom(K) -> K.

%% --

convert(Vs, {collection,Ts}) when is_list(Vs)   -> collection([],Vs,Ts);
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
    Value = translate(V,Ts),
    collection([Value|Acc],Vs,Ts).
