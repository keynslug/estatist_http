% @author Kozorezov Petr <petr.kozorezov@gmail.com>
% @copyright 2012 Kozorezov Petr
-module(estatist_http_handlers).
-behaviour(cowboy_http_handler).

%% Protocol:
%% -> Method: GET, URL: /
%% -> Method: GET, URL: /all
%% -> Method: GET, URL: /all/all
%% -> Method: GET, URL: /all/all/all
%% -> Method: GET, URL: /a,b,c
%% -> Method: GET, URL: /all/meter/one,five

%%
%% cowboy_http_handler behaviour
%%
-export([
         init/3,
         handle/2,
         terminate/2
        ]).

init({_Any, http}, Request, _Options) ->
    [Method, Path] = examine_request(Request, [method, path_info]),
    {ok, Request, {Method, Path}}.

handle(Request, {<<"GET">>, List}) ->
    {ok, Reply} = try
        [Names, Types, Params, RowID] = expand(4, undefined, List),
        Query = [ 
                    {names,   param(Names, all)},
                    {types,   param(Types, all)},
                    {params,  param(Params, all)}
                ],
        FinalQuery = case RowID of
            undefined ->
                Query;
            Value ->
                [{row_id, {id, Value}} | Query]
        end,
        case estatist:select(FinalQuery) of
            {ok, MetricsValues} ->
                ProparedForJson = encode_response(MetricsValues),
                Json = jiffy:encode(ProparedForJson),
                cowboy_req:reply(200, [{<<"Content-Type">>, "application/json"}], Json, Request);
            {error, Err} ->
                throw(Err)
        end
    catch
        throw:Error -> 
            error_logger:error_report(["Metrics selection failed", {reason, Error}]),
            reply(400, Request)
    end,
    {ok, Reply, undefined};

handle(Request, _) ->
    {ok, Reply} = reply(405, Request),
    {ok, Reply, undefined}.


terminate(_Request, _State) ->
    ok.


reply(Code, Request) ->
    cowboy_req:reply(Code, Request).


examine_request(Request, What) ->
    [ begin {Value, _} = cowboy_req:Ask(Request), Value end || Ask <- What ].


encode_response(List) when is_list(List) ->
    {[encode_response(E) || E <- List]};
encode_response({Name, Obj}) ->
    {encode_name(Name), encode_response(Obj)};
encode_response(V) ->
    V.


encode_name(List) when is_list(List) ->
    list_to_binary(List);
encode_name(Name) ->
    Name.


expand(0, _Value, []) ->
    [];
expand(0, _Value, _L) ->
    throw(badpath);
expand(N, Value, [H | T]) ->
    [H | expand(N - 1, Value, T)];
expand(N, Value, []) ->
    [Value | expand(N - 1, Value, [])].


param(undefined, Default) ->
    Default;

param(<<"*">>, Default) ->
    Default;

param(Field, _Default) ->
    case [make_field_atom(Value) || Value <- tokens(Field)] of
        [V] ->
            V;
        O ->
            O
    end.

tokens(Data) ->
    binary:split(Data, <<",">>, [global]).

make_field_atom(Field) ->
    try binary_to_existing_atom(Field, utf8)
    catch error:badarg -> throw({bad_request, {unknown_atom, Field}})
    end.
