% @author Kozorezov Petr <petr.kozorezov@gmail.com>
% @copyright 2012 Kozorezov Petr
-module(estatist_http_srv).

%%
%% API
%%
-export([
         init/1
        ]).

%%
%% API
%%

init(Options) ->
    %% ?LOG_INFO(" initializing..."),

    Host = proplists:get_value(host, Options, "0.0.0.0"),
    Port = proplists:get_value(port, Options, 8000),

    {ok, HostTuple} = inet:getaddr(Host, inet),
    TransportOptions = [ {ip, HostTuple}, {port, Port} ],

    Dispatch = cowboy_router:compile([
                {'_', [
                       {"/[...]", estatist_http_handlers, {}}
                      ]}
               ]),

    ranch:child_spec(
        ?MODULE, 8,
        ranch_tcp, TransportOptions,
        cowboy_protocol, [{env, [{dispatch, Dispatch}]}]
        ).

