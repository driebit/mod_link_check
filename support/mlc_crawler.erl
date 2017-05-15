%% @author Driebit <tech@driebit.nl>
%% @copyright 2017

-module(mlc_crawler).
-export([
    group_by_hostname/1,
    check_status/1,
    check/2,
    check/3,
    check_batch/2,
    spawn_checks/2
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

% @doc Checks the response status of a given URL
check_status(Url) when is_binary(Url) ->
    check_status(z_convert:to_list(Url));
check_status(Url) ->
    Resp = httpc:request(get, {Url, []}, [{autoredirect, false}], []),
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
            timer:sleep(Delay * 1000)
        end,
        Urls
    ).

check(Urls, FromPid) ->
    check(Urls, FromPid, 0).

check_batch([], _FromPid) ->
    ok;
check_batch(Urls, FromPid) ->

    % TODO: Make these configurable
    BatchSize = 100,
    BatchDelaySeconds = 10,
    HostDelaySeconds = 10,

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
            spawn(mlc_crawler, check, [HostUrls, FromPid, HostDelaySeconds])
        end,
        GroupedUrls
    ),

    % Pause before next batch
    timer:sleep(BatchDelaySeconds * 1000),
    check_batch(NextBatch, FromPid).

% @doc Spawns an async processes that crawl URLS
spawn_checks(Urls, FromPid) when is_list(Urls) ->

    % TODO: Figure out proper implementation that batches on:
    % 1. Number of parallel outgoing requests
    % 2. Numebr of concurrent requests to host

    % Don't check links twice
    UniqueUrls = remove_duplicates(Urls),
    spawn(mlc_crawler, check_batch, [UniqueUrls, FromPid]).
