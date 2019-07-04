%%%-------------------------------------------------------------------
%%% @author Kreastone
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Июль 2019 10:18
%%%-------------------------------------------------------------------
-module(www_profile).
-author("Kreastone").

-include("include.hrl").

%% API
%% API
-export([init/2,
  allowed_methods/2,
  is_authorized/2,
  options/2,
  content_types_provided/2,
  set_content/2,
  charsets_provided/2,
  content_types_accepted/2,
  accept/2]).

%%=======================================

init(Req0, State) ->
  {cowboy_rest, Req0, State}.

%%=======================================
%% Rest
%%=======================================

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req0, State) ->
  case cowboy_req:header(<<"auth_token">>, Req0) of
    undefined -> {{false, <<"User not found">>}, Req0, State};
    IdBinary -> try binary_to_integer(IdBinary) of
      Auth_Token ->
        case util:check_login(Auth_Token) of
          {ok, UID} -> {true, Req0, State#{uid => UID}};
          error -> {{false, <<"User not found">>}, Req0, State}
        end
      catch
        _ : _ -> {{false, <<"User not found">>}, Req0, State}
      end
  end.

options(Req0, State) ->
  Req = cowboy_req:set_resp_header(<<"allow">>, <<"GET, POST, OPTIONS">>, Req0),
  {ok, Req, State}.

content_types_provided(Req0, State) ->
  {[
    {<<"application/json">>, set_content}
  ], Req0, State}.

set_content(Req0, State) ->
  UID = maps:get(uid, State),
  case util:get_profile(UID) of
    error -> {<<"error">>, Req0, State};
    Profile ->  {
      jsx:encode(
        #{nickname => Profile#profile.nickname,
          coins => Profile#profile.coins,
          stars => Profile#profile.stars,
          level => Profile#profile.level
      }), Req0, State}
  end.

charsets_provided(Req, State) ->
  {[<<"utf-8">>], Req, State}.

content_types_accepted(Req0, State) ->
  {[
    {{<<"application">>,<<"json">>,[{<<"charset">>,<<"utf-8">>}]}, accept}
  ], Req0, State}.

accept(#{method := <<"GET">>} = Req0, State) ->
  {true, Req0, State};
accept(Req0, State) ->
  case cowboy_req:read_body(Req0) of
    {ok, Data, _} when size(Data) == 0 ->
      {false, Req0, State};
    {more, _More, _} ->
      {false, Req0, State};
    {ok, Data, _} ->
      handle_command(Data, Req0, State)
  end.

%% ====================================================

handle_command(Data, Req0, State) ->
  case get_key(Data, <<"command">>) of
    {ok, <<"buy_stars">>} ->
      buy_stars(Data, Req0, State);
    {ok, <<"win_level">>} ->
      win_level(Req0, State);
    {ok, <<"delete_profile">>} ->
      delete_profile(Req0, State);
    error ->
      {false, Req0, State}
  end.

delete_profile(Req0, State) ->
  UID = maps:get(uid, State),
  Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json;charset=utf-8">>, Req0),
  case util:delete_profile(UID) of
    ok ->
      Req2 = cowboy_req:set_resp_body(jsx:encode(#{status => ok}), Req1),
      {true, Req2, State};
    {error, Reason} ->
      Req2 = cowboy_req:set_resp_body(jsx:encode(#{status => error, reason => Reason}), Req1),
      {true, Req2, State}
  end.

win_level(Req0, State) ->
  UID = maps:get(uid, State),
  Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json;charset=utf-8">>, Req0),
  case util:win_level(UID) of
    {ok, Profile} ->
      Req2 = cowboy_req:set_resp_body(jsx:encode(#{status => ok, level => Profile#profile.level}), Req1),
      {true, Req2, State};
    {error, Reason} ->
      Req2 = cowboy_req:set_resp_body(jsx:encode(#{status => error, reason => Reason}), Req1),
      {true, Req2, State}
  end.

buy_stars(Data, Req0, State) ->
  UID = maps:get(uid, State),
  case get_key(Data, <<"stars_count">>) of
    {ok, Stars_Count} ->
      Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json;charset=utf-8">>, Req0),
      case util:buy_stars(UID, Stars_Count) of
        {ok, Profile} ->
          Req2 = cowboy_req:set_resp_body(jsx:encode(#{status => ok, stars => Profile#profile.stars}), Req1),
          {true, Req2, State};
        {error, Reason} ->
          Req2 = cowboy_req:set_resp_body(jsx:encode(#{status => error, reason => Reason}), Req1),
          {true, Req2, State}
      end;
    error ->
      {false, Req0, State}
  end.

get_key(Data, Key) ->
  try
    Map = jsx:decode(Data, [return_maps]),
    Value = maps:get(Key, Map),
    {ok, Value}
  catch
    _:_ -> error
  end.