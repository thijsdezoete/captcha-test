%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc mymochiapp.

-module(mymochiapp).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the mymochiapp server.
start() ->
    mymochiapp_deps:ensure(),
    ensure_started(crypto),
    application:start(emysql),
    application:start(mymochiapp).


%% @spec stop() -> ok
%% @doc Stop the mymochiapp server.
stop() ->
    application:stop(emysql),
    application:stop(mymochiapp).
