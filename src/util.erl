%%%-------------------------------------------------------------------
%%% @author Kreastone
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Июль 2019 9:29
%%%-------------------------------------------------------------------
-module(util).
-author("Kreastone").

-include("include.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([register/1, login/1, check_login/1, timer_ttl/0, get_profile/1, buy_stars/2, win_level/1, delete_profile/1]).

timer_ttl() ->
  1.

-spec(delete_profile(integer()) -> {ok, any()} | {error, binary()}).
delete_profile(UID) ->
  F =
    fun() ->
      mnesia:delete({profile, UID}),
      Query = qlc:q([X || X <- mnesia:table(login), X#login.uid == UID]),
      Logins = qlc:e(Query),
      lists:foldl(fun(Entry, _Acc) ->
        mnesia:delete({login, Entry#login.auth_token})
                  end, [], Logins)
    end,

  case mnesia:transaction(F) of
    {atomic, _} -> ok;
    _ -> {error, <<"mnesia error">>}
  end.

%%====================================================================

-spec(win_level(integer()) -> {ok, any()} | {error, binary()}).
win_level(UID) ->
  F =
    fun() ->
      case mnesia:read({profile, UID}) of
        [Profile] ->
          if
            (Profile#profile.level < Profile#profile.stars) ->
              New_Profile = Profile#profile{
                level = Profile#profile.level + 1},
              mnesia:write(New_Profile),
              {ok, New_Profile};
            true ->
              {error, <<"not enough stars">>}
          end
      end
    end,
  case mnesia:transaction(F) of
    {atomic, Result} -> Result;
    _ -> {error, <<"mnesia error">>}
  end.

%%====================================================================

-spec(buy_stars(integer(), integer()) -> {ok, any()} | {error, binary()}).
buy_stars(UID, Count) when is_integer(Count)->
  F =
    fun() ->
      case mnesia:read({profile, UID}) of
        [Profile] ->
          if
            (Profile#profile.coins >= 10 * Count) ->
              New_Profile = Profile#profile{
                coins = Profile#profile.coins - 10 * Count,
                stars = Profile#profile.stars + Count},
              mnesia:write(New_Profile),
              {ok, New_Profile};
            true ->
              {error, <<"not enough money">>}
          end
      end
    end,
  case mnesia:transaction(F) of
    {atomic, Result} -> Result;
    _ -> {error, <<"mnesia error">>}
  end.

%%====================================================================

-spec(get_profile(integer()) -> any()).
get_profile(UID) ->
  F =
    fun() ->
      mnesia:read({profile, UID})
    end,
  case mnesia:transaction(F) of
    {atomic, [Profile]} -> Profile;
    _ -> error
  end.

%%====================================================================

-spec(check_login(integer()) -> {ok, integer()} | error).
check_login(Auth_Token) when is_integer(Auth_Token) ->
  F =
    fun() ->
      case mnesia:read({login, Auth_Token}) of
        [Login] ->
          {ok, Login#login.uid};
        _ -> error
      end
    end,

  case mnesia:transaction(F) of
    {atomic, Result} -> Result;
    _ -> error
  end;
check_login(_) ->
  error.

%%====================================================================

-spec(login(binary()) -> {ok, binary()} | {error, any()}).
login(UID) when is_integer(UID) ->
  F =
    fun() ->
      case mnesia:read({profile, UID}) of
        [] ->
          {error, <<"user with such id not found">>};
        _ ->
          Auth_Token = get_new_id(login),
          mnesia:write(#login{auth_token = Auth_Token, uid = UID}),
          {ok, Auth_Token}
      end
    end,

  case mnesia:transaction(F) of
    {atomic, Result} -> Result;
    {aborted, _} -> {error, <<"mnesia error">>}
  end;
login(_) ->
  {error, <<"bad request">>}.

%%====================================================================

-spec(register(binary()) -> {ok, binary()} | {error, any()}).
register(Nickname) when is_binary(Nickname) ->
  F =
    fun() ->
      UID = get_new_id(profile),
      mnesia:write(#profile{uid=UID, nickname=Nickname}),
      UID
    end,

  case mnesia:transaction(F) of
    {atomic, UID} -> {ok, UID};
    {aborted, Reason} -> {error, Reason}
  end;
register(_) ->
  {error, <<"bad request">>}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec(get_new_id(atom()) -> integer()).
get_new_id(Table) ->
  ID = rand:uniform(134217728),
  case mnesia:read({Table, ID}) of
    [] -> ID;
    _ -> get_new_id(Table)
  end.