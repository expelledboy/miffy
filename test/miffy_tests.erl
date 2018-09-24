-module(miffy_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

decode_test() ->
    Json = atom_to_binary('{"key":"set","value":{"name":"expelledboy","level":9000}}', latin1),
    Types = #{ key => atom },
    ?assertMatch(#{ key := "set", value := #{ name := "expelledboy", level := 9000 } }, miffy:decode(Json)),
    ?assertMatch(#{ key := set, value := #{ name := "expelledboy", level := 9000 } }, miffy:decode(Json, Types)).

non_object_test() ->
    ?assertEqual(1, miffy:decode(<<"1">>)),
    ?assertEqual(true, miffy:decode(<<"true">>)),
    ?assertEqual([], miffy:decode(<<"[]">>)),
    ?assertEqual("string", miffy:decode(<<"'string'">>)).

data_format_test() ->
    Terms = #{ integer => 1,
               float => 1.0,
               atom => symbol,
               binary => <<"binary">>,
               string => "value",
               obj => #{} },
    TermTypes = #{ atom => atom,
                   binary => binary,
                   string => string },
    Erlang = Terms#{ collection => lists:duplicate(10, Terms),
                     sub => Terms },
    Types = TermTypes#{ collection => {collection, TermTypes},
                        sub => TermTypes },
    Json = miffy:encode(Erlang, Types),
    ?assertEqual(Erlang, miffy:decode(Json, Types)).

list_transation_test() ->
    In = #{ list => [ atom,
                      "string",
                      <<"binary">>,
                      #{ obj => value } ] },
    Out = #{list => [<<"atom">>,"string",<<"binary">>, #{<<"obj">> => <<"value">>}]},
    Json = miffy:encode(In),
    ?assertEqual(Out, miffy:decode(Json)).
