%% @author Mochi Media <dev@mochimedia.com>
%% @copyright mymochiapp Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the mymochiapp application.

-module(mymochiapp_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mymochiapp.
start(_Type, _StartArgs) ->
    mymochiapp_deps:ensure(),
    mymochiapp_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mymochiapp.
stop(_State) ->
    ok.
