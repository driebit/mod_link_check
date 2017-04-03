%% @author Driebit <tech@driebit.nl>
%% @copyright 2017

-module(mlc_crawler).
-export([
    check_status/1,
    check/2,
    spawn_check/2
]).

-include_lib("zotonic.hrl").

% @doc Checks the response status of a given URL
check_status(Url) when is_binary(Url) ->
    check_status(z_convert:to_list(Url));
check_status(Url) ->
    case httpc:request(Url) of
        {ok, {{_,Status,_},_,_}} -> Status;
        {error, Reason} -> Reason
    end.

% @doc Checks and sends the response back to the module Pid
check(Url, FromPid) ->
    Status = check_status(Url),
    gen_server:cast(FromPid, {update_status, [Url, Status]}).

% @doc Spawns an async process that crawls one or more URLS
spawn_check(Url, FromPid) when is_binary(Url) ->
    spawn(mlc_crawler, check, [Url, FromPid]);

spawn_check([H|_] = Url, FromPid) when is_integer(H) ->
    spawn(mlc_crawler, check, [Url, FromPid]);

spawn_check(Urls, FromPid) when is_list(Urls) ->
    lists:foreach(
        fun(Url) -> spawn_check(Url, FromPid) end,
        Urls
    ).
