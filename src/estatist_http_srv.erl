% @author Kozorezov Petr <petr.kozorezov@gmail.com>
% @copyright 2012 Kozorezov Petr
-module(estatist_http_srv).
-behaviour(gen_server).

%%
%% API
%%
-export([
         start_link/0,
         start_link/1
        ]).

%%
%% gen_server callbacks
%%
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%%
%% API
%%

start_link() ->
    start_link([]).

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%
%% gen_server callbacks
%%

init(Options) ->
    %% ?LOG_INFO(" initializing..."),

    Host = proplists:get_value(host, Options, "0.0.0.0"),
    Port = proplists:get_value(port, Options, 8000),

    {ok, HostTuple} = inet:getaddr(Host, inet),
    TransportOptions = [ {ip, HostTuple}, {port, Port} ],

    Dispatch = [
                {'_', [
                       {['...'], estatist_http_handlers, {}}
                      ]}
               ],
    {ok, _Pid} = cowboy:start_http(?MODULE, 8, TransportOptions, [{dispatch, Dispatch}]),
    {ok, undefined}.

handle_call(_Msg, _From, State) ->
    %% ?LOG_ERROR("unexpected call '~p' from ~p", [Msg, From]),
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    %% ?LOG_ERROR("unexpected cast '~p'", [Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    %% ?LOG_ERROR("unexpected info '~p'", [Info]),
    {noreply, State}.

terminate(_, _) ->
    %% ?LOG_INFO("terminating..."),
    ok = cowboy:stop_listener(?MODULE).

code_change(_, State, _) ->
    {ok, State}.

