%% @author Driebit <tech@driebit.nl>
%% @copyright 2017

-module(mlc_crawler).
-export([
    group_by_hostname/1,
    check_status/1,
    check/2,
    check/3,
    check_batch/2,
    spawn_checks/2,
    periodic_cleanup/0
]).

-include_lib("zotonic.hrl").

% @doc Generic group_by function
group_by(GroupFun, List) ->
    lists:foldl(
        fun(Elm, Acc) ->
            GroupKey = GroupFun(Elm),
            CurrentGroupVal = proplists:get_value(GroupKey, Acc, []),
            UpdateGroupVal = [Elm|CurrentGroupVal],
            FlushedAcc = proplists:delete(GroupKey, Acc),
            [{GroupKey, UpdateGroupVal}|FlushedAcc]
        end,
        [],
        List
    ).

% @doc Removes duplicates but keeps order
remove_duplicates(List) ->
    FilteredArgs = lists:foldl(
        fun(Arg, Acc) ->
           case lists:member(Arg, Acc) of
               true -> Acc;
               false -> [Arg|Acc]
           end
        end,
        [],
        List
    ),
    lists:reverse(FilteredArgs).

% @doc Group Urls by hostname
group_by_hostname(Urls) ->
    group_by(
        fun(Url) ->
            {_, Host, _, _, _} = mochiweb_util:urlsplit(z_convert:to_list(Url)),
            Host
        end,
        Urls
    ).

ensure_profile() ->
    case inets:start(httpc, [{profile, mlc_crawler}]) of
        {ok, _} ->
            ok = httpc:set_options([
                {max_sessions, 10},
                {max_keep_alive_length, 10},
                {keep_alive_timeout, 20000},
                {cookies, enabled}
            ], mlc_crawler),
            periodic_cleanup(),
            ok;
        {error, {already_started, _}} -> ok
    end.

periodic_cleanup() ->
    httpc:reset_cookies(mlc_crawler),
    {ok, _} = timer:apply_after(timer:hours(1), ?MODULE, periodic_cleanup, []),
    ok.

% @doc Checks the response status of a given URL
check_status(Url) when is_binary(Url) ->
    check_status(z_convert:to_list(Url));
check_status(Url) ->
    ensure_profile(),
    Resp = httpc:request(get, {Url, []}, [{autoredirect, false}], [], mlc_crawler),
    case Resp of
        {ok, {{_,Status,_},_,_}} -> Status;
        {error, Reason} -> Reason
    end.

% @doc Checks and sends the response back to the module Pid
check(Urls, FromPid, Delay) ->
    lists:foreach(
        fun(Url) ->
            Status = check_status(Url),
            gen_server:cast(FromPid, {update_status, [Url, Status]}),
            timer:sleep(Delay)
        end,
        Urls
    ).

check(Urls, FromPid) ->
    check(Urls, FromPid, 0).

check_batch([], _FromPid) ->
    ok;
check_batch(Urls, FromPid) ->

    % TODO: Make these configurable

    % Never start more than BatchSize concurrent requests
    BatchSize = 100,

    % Wait HostDelaySeconds for next request to same host
    HostDelay = timer:seconds(20),

    % Wait for BatchDelaySeconds to start next batch of requests
    BatchDelay = HostDelay * BatchSize,

    LengthUrls = length(Urls),
    LimitedBatchSize = case BatchSize > LengthUrls of
        true -> LengthUrls;
        false -> BatchSize
    end,

    {ThisBatch, NextBatch} = lists:split(LimitedBatchSize, Urls),

    % When checking multiple URLS batch by hostname
    GroupedUrls = group_by_hostname(ThisBatch),
    lists:foreach(
        fun({_Host, HostUrls}) ->
            spawn(mlc_crawler, check, [HostUrls, FromPid, HostDelay])
        end,
        GroupedUrls
    ),

    % Pause before next batch
    timer:sleep(BatchDelay),
    check_batch(NextBatch, FromPid).

% @doc Spawns an async processes that crawl URLS
spawn_checks(Urls, FromPid) when is_list(Urls) ->

    % TODO: Figure out proper implementation that batches on:
    % 1. Number of parallel outgoing requests
    % 2. Numebr of concurrent requests to host

    % Don't check links twice
    UniqueUrls = remove_duplicates(Urls),
    spawn(mlc_crawler, check_batch, [UniqueUrls, FromPid]).
