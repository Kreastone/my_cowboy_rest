%%%-------------------------------------------------------------------
%%% @author Kreastone
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Июль 2019 9:30
%%%-------------------------------------------------------------------
-module(www_login).
-author("Kreastone").

%% API
-export([init/2,
  allowed_methods/2,
  is_authorized/2,
  options/2,
  content_types_accepted/2,
  login/2]).

%%=======================================

init(Req0, State) ->
  {cowboy_rest, Req0, State}.

%%=======================================
%% Rest
%%=======================================

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req0, State) ->
  {true, Req0, State}.

options(Req0, State) ->
  Req = cowboy_req:set_resp_header(<<"allow">>, <<"POST, OPTIONS">>, Req0),
  {ok, Req, State}.

content_types_accepted(Req0, State) ->
  {[
    {{<<"application">>,<<"json">>,[{<<"charset">>,<<"utf-8">>}]}, login}
  ], Req0, State}.

login(Req0, State) ->
  case cowboy_req:read_body(Req0) of
    {ok, Data, _} when size(Data) == 0 ->
      {false, Req0, State};
    {more, _More, _} ->
      {false, Req0, State};
    {ok, Data, _} -> case get_uid(Data) of
      {ok, UID} ->
       Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json;charset=utf-8">>, Req0),
       case util:login(UID) of
         {ok, Auth_Token} ->
           Req2 = cowboy_req:set_resp_body(jsx:encode(#{auth_token => Auth_Token}), Req1),
           {true, Req2, State};
         {error, Reason} ->
           Req2 = cowboy_req:set_resp_body(jsx:encode(#{error => Reason}), Req1),
           {false, Req2, State}
       end;
      error ->
       {false, Req0, State}
      end
  end.

get_uid(Data) ->
  try
    Map = jsx:decode(Data, [return_maps]),
    UID = maps:get(<<"uid">>, Map),
    {ok, UID}
  catch
    _:_ -> error
  end.