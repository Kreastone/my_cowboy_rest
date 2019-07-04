%%%-------------------------------------------------------------------
%% @doc test_zimad public API
%% @end
%%%-------------------------------------------------------------------

-module(test_zimad_app).

-behaviour(application).
-include("include.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_httpd(),
    database_init(),
    test_zimad_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_httpd() ->
    ListRoute = get_httpd_route(),
    Routing = cowboy_router:compile([{'_', ListRoute}]),
    {ok, WWW_TLS} = application:get_env(?APPLICATION, www_tls),
    {FunStart, Config} = case WWW_TLS of
                             true -> {fun cowboy:start_tls/3, [
                                 {certfile, ?certfile},
                                 {keyfile, ?keyfile}
                             ]};
                             false -> {fun cowboy:start_clear/3, []}
                         end,
    {ok, WWW_Port} = application:get_env(?APPLICATION, www_port),
    FullConf = Config ++ [{port, WWW_Port}],
    Env = #{env => #{dispatch => Routing}, server => "minigame"},
    case FunStart(?HTTP_LISTEN, FullConf, Env) of
        {ok, _} ->
            error_logger:info_msg("HTTP server started.", []);
        {error, {already_started, _}} ->
            error_logger:info_msg("HTTP server already started.", []);
        {error, Reason} ->
            error_logger:error_msg("HTTP server not started: ~p", [Reason])
    end.

get_httpd_route() ->
    {ok, WWW_dir} = application:get_env(?APPLICATION, www_dir),
    F = fun
            ({PathMatch, cowboy_static, {Type, FilePath, Conf}}, AccIn) ->
                AccIn ++ [{PathMatch, cowboy_static, {Type, WWW_dir ++ FilePath, Conf}}];
            (Path, AccIn) -> AccIn ++ [Path]
        end,
    case file:consult(code:priv_dir(?APPLICATION) ++ "/http_route") of
        {ok, [Terms | _]} -> lists:foldl(F, [], Terms);
        {error, Reason} -> mnesia:abort(?txt("HTTP server not started: ~p", [Reason]))
    end.

database_init() ->
    mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(login, [{ram_copies, [node()]}, {attributes, record_info(fields, login)}]),
    mnesia:create_table(profile, [{disc_copies, [node()]}, {attributes, record_info(fields, profile)}]),
    timer:apply_interval(1000, util, timer_ttl, []).