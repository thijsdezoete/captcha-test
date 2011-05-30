%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for mymochiapp.

-module(mymochiapp_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "captcha" ->
                        {CodeHex, BinPng} = mycaptcha:new(),
                        Cookie = mochiweb_cookies:cookie("cap", CodeHex, []),
                        Req:ok({"image/png", [Cookie], [BinPng]});

                    "Test" ->
                        io:format("Test called~nPath: ~p~n", [Req]),
                        Req:serve_file(Path, DocRoot);
                    _ ->
                        
                        % Example mysql connection to do SOMETHING!

                        %emysql:add_pool(hello_pool, 1, 
                        %    "erlang", "X2qhZ77F9zpBFVqf", "localhost", 3306,
                        %    "erlang_mochi", utf8),

                        %emysql:execute(hello_pool,
                        %    <<"insert into test_table (`code`, `hash`) values ('blaat', 'blaat2');">>),

                        %io:format("Emysql pool: ~p~nTest: ~p~n", [Pool, Test]),

                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    "captcha" ->
                        CodeHex = Req:get_cookie_value("cap"),
                        DataIn = Req:parse_post(),
                        CapCode = proplists:get_value("capCode", DataIn),

                        case mycaptcha:check(CodeHex, CapCode) of
                            true ->
                                Req:ok({"text/html", [], ["<h1>Code Correct!</h1>"]});
                            false ->
                                Req:ok({"text/html", [], ["<h1 style=\"color: red;\">Incorrect Code!</h1>"]})
                        end;
                            
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
