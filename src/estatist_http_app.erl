% @author Kozorezov Petr <petr.kozorezov@gmail.com>
% @copyright 2012 Kozorezov Petr
-module(estatist_http_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%
%% Application callbacks
%% 

start(_StartType, _StartArgs) ->
    estatist_http_sup:start_link().

stop(_State) ->
    ok.
