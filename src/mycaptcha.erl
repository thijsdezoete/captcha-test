-module(mycaptcha).
-compile(export_all).

new() ->
    CryptKey = case ets:info(captcha) of 
        undefined ->
            captcha = ets:new(captcha, [set, public, named_table]),
            CK = crypto:rand_bytes(16),
            true = ets:insert(captcha, {CK}),
            CK;

        _Info ->
            ets:first(captcha)
    end,

    FileName = lists:flatmap(
        fun(Item) -> integer_to_list(Item) end, 
        tuple_to_list(now())
    ),

    Code = generate_rand(5),

    File = io_lib:format("/tmp/~s.png", [FileName]),
    Cmd = io_lib:format("convert -background 'none' -fill '#222222' -size 175 -gravity Center -wave 5x100 -swirl 20 -font DejaVu-Serif-Book -pointsize 28 label:~s -draw 'Bezier 10,40 50,35 100,35 150,35 200,50 200,35 300 35' ~s", [Code, File]),
    os:cmd(Cmd),

    {ok, BinPng} = file:read_file(File),
    file:delete(File),

    Sha = crypto:sha_mac(CryptKey, integer_to_list(lists:sum(Code)) ++ Code),
    CodeHex = mochihex:to_hex(Sha),
    
    % Add to DB
    %application:start(emysql),
    case emysql_conn_mgr:pools() of 
        [] -> 
            emysql:add_pool(hello_pool, 1, 
        "erlang", "password", "localhost", 3306,
        "erlang_mochi", utf8);
        Pools ->
            case lists:keysearch(hello_pool, 2, Pools) of 
                false ->
                    emysql:add_pool(hello_pool, 1, 
                        "erlang", "password", "localhost", 3306,
                        "erlang_mochi", utf8);
                _ ->
                    ok
            end
    end,


    Query = io_lib:format("insert into test_table (`code`, `hash`) values ('~s', '~s');",[Code, CodeHex]),
    io:format("Query: ~s~n", [Query]),

    emysql:execute(hello_pool,
        list_to_binary(Query)),

    {CodeHex, BinPng}.

check(CodeHex, Code) ->
    Sha = mochihex:to_bin(CodeHex),
    CryptKey = ets:first(captcha),

    case crypto:sha_mac(CryptKey, integer_to_list(lists:sum(Code)) ++ Code) of
        Sha ->
            true;

        _ ->
            false
    end.

generate_rand(Length) -> 
    Now = now(),
    random:seed(element(1,Now), element(2, Now), element(3, Now)),
    lists:foldl(fun(_I, Acc) -> [do_rand(0) | Acc] end, [], lists:seq(1, Length)).

do_rand(R) when R > 46, R < 48; R > 64, R < 91; R > 96 ->
    R;
do_rand(_R) ->
    do_rand(47 + random:uniform(75)).

