-module(miffy_utils).
-compile(export_all).

parser() ->
    case find_parser() of
        undefined -> throw(json_parser_not_found);
        Parser -> Parser
    end.

find_parser() ->
    case application:get_env(?MODULE, library, undefined) of
        undefined ->
            Modules = [code:ensure_loaded(jiffy), code:ensure_loaded(jsx)],
            Parser = proplists:get_value(module, Modules),
            ok = application:set_env(?MODULE, library, Parser),
            Parser;
        Parser ->
            Parser
    end.
