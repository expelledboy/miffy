-module(miffy_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

decode_test() ->
    Json = atom_to_binary('{"key":"set","value":{"name":"expelledboy","level":9000}}', latin1),
    Types = #{ key => atom },
    ?assertMatch(#{ key := "set", value := #{ name := "expelledboy", level := 9000 } }, miffy:decode(Json)),
    ?assertMatch(#{ key := set, value := #{ name := "expelledboy", level := 9000 } }, miffy:decode(Json, Types)).

data_format_test() ->
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
    ?assertMatch(Erlang, miffy:decode(Json, Types)).

list_transation_test() ->
    Erlang = #{ list => [ atom,
                          "string",
                          <<"binary">>,
                          #{ obj => value } ] },
    Special = #{list => [<<"atom">>,"string",<<"binary">>, #{<<"obj">> => <<"value">>}]},
    Json = miffy:encode(Erlang),
    ?assertMatch(Special, miffy:decode(Json)).
